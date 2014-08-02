# Copyright 2014 Google Inc. All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


# The CausalImpact package implements inference on the causal effect of an
# intervention on a time series. It uses a counterfactual-forecasting strategy
# based on a Bayesian structural time-series model.
#
# Literature:
#   Brodersen KH, Gallusser F, Koehler J, Remy N, Scott SL (under review).
#   Inferring causal impact using Bayesian structural time-series models.
#   http://research.google.com/pubs/pub41854.html
#
# Author: kbrodersen@google.com (Kay Brodersen)

# Specify defaults for <model.args>
# (should always be in sync with the documentation of CausalImpact())
.defaults <- list(niter = 1000,
                  standardize.data = TRUE,
                  prior.level.sd = 0.01,
                  nseasons = 1,
                  season.duration = 1,
                  dynamic.regression = FALSE)

# ------------------------------------------------------------------------------
FormatInputData <- function(data) {
  # Checks and formats the <data> argument provided to CausalImpact().
  #
  # Args:
  #   data: zoo object or data frame
  #
  # Returns:
  #   correctly formatted zoo object

  # If <data> is a data frame with a 'date' column, try to convert
  if (is.data.frame(data) && tolower(names(data)[1]) %in% c("date", "time")) {
    if (class(data$date) == "Date") {
      data <- zoo(data[, -1], data$date)
    } else {
      warning(paste0("Did you mean: data = zoo(data[, -1], data$",
                     names(data)[1]))
    }
  }

  # Try to convert to zoo object
  data <- TryStop(as.zoo(data), "could not convert input data to zoo object")
  
  # Ensure <data> is formatted in such a way that rows represent time points
  if (is.null(ncol(data))) {
    dim(data) <- c(length(data), 1)
  }

  # Must have at least 3 time points
  assert_that(nrow(data) > 3)

  # Must not have NA in covariates (if any)
  if (ncol(data) >= 2) {
    assert_that(!any(is.na(data[, -1])))
  }
  return(data)
}

# ------------------------------------------------------------------------------
FormatInputPrePostPeriod <- function(pre.period, post.period, data) {
  # Checks and formats the <pre.period> and <post.period> input arguments.
  #
  # Args:
  #   pre.period: two-element vector
  #   post.period: two-element vector
  #   data: already-checked zoo object, for reference only

  assert_that(!is.null(pre.period))
  assert_that(!is.null(post.period))
  assert_that(length(pre.period) == 2, length(post.period) == 2)
  assert_that(!any(is.na(pre.period)), !any(is.na(post.period)))
  if (class(time(data)) != class(pre.period) ||
      class(time(data)) != class(post.period)) {
    if (class(time(data)) == "integer") {
      pre.period <- as.integer(pre.period)
      post.period <- as.integer(post.period)
    } else if (class(time(data)) == "numeric") {
      pre.period <- as.numeric(pre.period)
      post.period <- as.numeric(post.period)
    } else {
      stop(paste0("pre.period (", class(pre.period), ") and post.period (",
                  class(post.period), ") should have the same class as the ",
                  "time points in the data (", class(time(data)), ")"))
    }
  }
  if (pre.period[1] < start(data)) {
    warning(paste0("Setting pre.period[1] to start of data: ", start(data)))
    pre.period[1] <- start(data)
  }
  if (pre.period[2] > end(data)) {
    warning(paste0("Setting pre.period[2] to end of data: ", end(data)))
    pre.period[2] <- end(data)
  }
  if (post.period[2] > end(data)) {
    warning(paste0("Setting post.period[2] to end of data: ", end(data)))
    post.period[2] <- end(data)
  }
  assert(pre.period[2] - pre.period[1] + 1 >= 3,
         "pre.period must span at least 3 time points")
  assert_that(post.period[2] >= post.period[1])
  assert_that(post.period[1] > pre.period[2])
  return(list(pre.period = pre.period, post.period = post.period))
}

# ------------------------------------------------------------------------------
FormatInputForCausalImpact <- function(data, pre.period, post.period,
                                       model.args, bsts.model, y.post, alpha) {
  # Checks and formats all input arguments supplied to CausalImpact(). See the
  # documentation of CausalImpact() for details.
  #
  # Args:
  #   data:        zoo object or data frame
  #   pre.period:  beginning and end of pre-period
  #   post.period: beginning and end of post-period
  #   model.args:  list of additional arguments for the model
  #   bsts.model:  fitted bsts model (instead of data)
  #   y.post:      observed response in the post-period
  #   alpha:       tail-area for posterior intervals
  #
  # Returns:
  #   list of checked (and possibly reformatted) input arguments

  # Check that a consistent set of variables has been provided
  assert(xor(!is.null(data) && !is.null(pre.period) && !is.null(post.period) &&
             is.null(bsts.model) && is.null(y.post),
             is.null(data) && is.null(pre.period) && is.null(post.period) &&
             !is.null(bsts.model) && !is.null(y.post)),
         paste0("must either provide data, pre.period, post.period, model.args",
                "; or bsts.model and y.post"))

  # Check <data> and convert to zoo, with rows representing time points
  if (!is.null(data)) {
    data <- FormatInputData(data)
  }

  # Check <pre.period> and <post.period>
  if (!is.null(data)) {
    checked <- FormatInputPrePostPeriod(pre.period, post.period, data)
    pre.period <- checked$pre.period
    post.period <- checked$post.period
  }

  # Parse <model.args>, fill gaps using <.defaults>
  model.args <- ParseArguments(model.args, .defaults)
  #
  # Check only those parts of <model.args> that are used in this file. The other
  # fields will be checked in FormatInputForConstructModel().
  #
  # Check <standardize.data>
  assert_that(length(model.args$standardize.data) == 1)
  assert_that(is.logical(model.args$standardize.data))
  assert_that(!is.na(model.args$standardize.data))

  # Check <bsts.model>
  if (!is.null(bsts.model)) {
    assert_that(class(bsts.model) == "bsts")
  }

  # Check <y.post>
  if (!is.null(bsts.model)) {
    assert_that(!is.null(y.post), is.vector(y.post), is.numeric(y.post))
  }

  # Check <alpha>
  assert_that(is.numeric(alpha))
  assert_that(length(alpha) == 1)
  assert_that(!is.na(alpha))
  assert_that(alpha > 0, alpha < 1)

  # Return updated arguments
  return(list(data = data, pre.period = pre.period, post.period = post.period,
              model.args = model.args, bsts.model = bsts.model, y.post = y.post,
              alpha = alpha))
}

# ------------------------------------------------------------------------------
CausalImpact <- function(data = NULL,
                         pre.period = NULL,
                         post.period = NULL,
                         model.args = NULL,
                         bsts.model = NULL,
                         y.post = NULL,
                         alpha = 0.05) {
  # CausalImpact() performs causal inference through counterfactual
  # predictions using a Bayesian structural time-series model.
  #
  # Detailed and up-to-date documentation is provided in
  # ../man/CausalImpact.Rd. Type ?CausalImpact to display the documentation.
  # For example code, see the package vignette (go/causalimpact-vignette).
  #
  # Args:
  #   data:        Time series of response variable and any covariates. This can
  #                be a \code{zoo} object; a \code{vector}; a \code{matrix}; or
  #                a \code{data.frame}. In any of these cases, the response
  #                variable must be in the first column, and any covariates in
  #                subsequent columns. A \code{zoo} object is recommended, as
  #                its time indices will be used to format the x-axis in
  #                \code{plot()}.
  #
  #   pre.period:  A vector of two indices specifying the first and the last
  #                time point of the pre-intervention period in the response
  #                vector \code{y}. This period can be thought of as a training
  #                period, used to determine the relationship between the
  #                response variable and the covariates.
  #
  #   post.period: A vector of two indices specifying the first and the last day
  #                of the post-intervention period we wish to study. This is the
  #                period after the intervention has begun whose effect we are
  #                interested in. The relationship between response variable and
  #                covariates, as determined during the pre-period, will be used
  #                to predict how the response variable should have evolved
  #                during the post-period had no intervention taken place.
  #
  #   model.args:  Optional arguments that can be used to adjust the default
  #                construction of the state-space model used for inference.
  #                For full control over the model, you can construct your own
  #                model using the \code{bsts} package and feed the fitted model
  #                into \code{CausalImpact()} (see examples).
  #
  #   bsts.model:  Instead of passing in \code{data} and having
  #                \code{CausalImpact()} construct a model, it is possible to
  #                construct a model yourself using the \code{bsts} package. In
  #                this case, omit \code{data}, \code{pre.period}, and
  #                \code{post.period}. Instead only pass in \code{bsts.model},
  #                \code{y.post}, and \code{alpha} (optional). The model must
  #                have been fitted on data where the response variable was set
  #                to \code{NA} during the post-treatment period. The actual
  #                observed data during this period must then be passed to the
  #                function in \code{y.post}.
  #
  #   y.post:      Actual observed data during the post-intervention period.
  #                This is required if and only if a fitted \code{bsts.model} is
  #                passed instead of \code{data}.
  #
  #   alpha:       Desired tail-area probability for posterior intervals.
  #                Defaults to 0.05, which will produce central 95\% intervals.
  #
  # Returns:
  #   A CausalImpact object. This is a list of:
  #     series:   observed data, counterfactual, pointwise and cumulative impact
  #     summary:  summary table
  #     protocol: verbal description of the analysis
  #     model:    contains bsts.model, the fitted model returned by bsts()
  #
  # Optional arguments for model.args:
  #   niter:              number of MCMC iterations
  #   standardize.data:   whether to standardize the data before model fitting
  #   prior.level.sd:     standard deviation of the prior on the local level
  #   nseasons:           number of seasons in the seasonal component
  #   season.duration:    duration of each season
  #   dynamic.regression: whether to have dynamic instead of static coefficients
  #
  # For more details on all of the above, see the package manual (?CausalImpact)
  # or the vignette.
  #
  # Examples:
  #   set.seed(1)
  #   x1 <- 100 + arima.sim(model = list(ar = 0.999), n = 100)
  #   y <- x1 + rnorm(100)
  #   y[71:100] <- y[71:100] + 10
  #   data <- cbind(y, x1)
  #   pre.period <- c(1, 70)
  #   post.period <- c(71, 100)
  #
  #   impact <- CausalImpact(data, pre.period, post.period)
  #
  #   summary(impact)
  #   summary(impact, "protocol")
  #   plot(impact)
  #
  #   y.post <- y[post.period[1] : post.period[2]]
  #   y[post.period[1] : post.period[2]] <- NA
  #   ss <- AddLocalLevel(list(), y)
  #   bsts.model <- bsts(y ~ x1, ss, niter = 1000)
  #   impact <- CausalImpact(bsts.model = bsts.model, y.post = y.post)

  # Check input
  checked <- FormatInputForCausalImpact(data, pre.period, post.period,
                                        model.args, bsts.model, y.post, alpha)
  data <- checked$data
  pre.period <- checked$pre.period
  post.period <- checked$post.period
  model.args <- checked$model.args
  bsts.model <- checked$bsts.model
  y.post <- checked$y.post
  alpha <- checked$alpha

  # Depending on input, dispatch to the appropriate Run* method()
  if (!is.null(data)) {
    impact <- RunWithData(data, pre.period, post.period, model.args, alpha)
  } else {
    impact <- RunWithBstsModel(bsts.model, y.post, alpha)
  }
  return(impact)
}

# ------------------------------------------------------------------------------
RunWithData <- function(data, pre.period, post.period, model.args, alpha) {
  # Runs an impact analysis on top of a fitted bsts model.
  #
  # Args:
  #   data:        zoo object of response variable and covariates
  #   pre.period:  two-element vector specifying the pre-period limits
  #   post.period: two-element vector specifying the post-period limits
  #   model.args:  list of model arguments
  #   alpha:       tail-probabilities of posterior intervals
  #
  # Returns:
  #   See CausalImpact().

  # Zoom in on data in modeling range
  pre.period[1] <- max(pre.period[1], which(!is.na(data[, 1]))[1])
  data.modeling <- window(data, start = pre.period[1], end = post.period[2])
  if (is.null(ncol(data.modeling))) {
    dim(data.modeling) <- c(length(data.modeling), 1)
  }

  # Standardize all variables?
  UnStandardize <- identity
  if (model.args$standardize.data) {
    sd.results <- StandardizeAllVariables(data.modeling)
    data.modeling <- sd.results$data
    UnStandardize <- sd.results$UnStandardize
  }

  # Set observed response in post-period to NA
  window(data.modeling[, 1], start = post.period[1]) <- NA

  # Construct model and perform inference
  bsts.model <- ConstructModel(data.modeling, model.args)

  # Compile posterior inferences
  if (!is.null(bsts.model)) {
    y.post <- window(data[, 1], start = post.period[1], end = post.period[2])
    inferences <- CompilePosteriorInferences(bsts.model, y.post, alpha,
                                             UnStandardize)
  } else {
    inferences <- CompileNaInferences(data[, 1])
  }

  # Extend <series> to cover original range (padding with NA as necessary)
  empty <- zoo(, time(data))
  inferences$series <- merge(inferences$series, empty, all = TRUE)
  assert_that(nrow(inferences$series) == nrow(data))

  # Replace <y.model> by full original response
  inferences$series <- cbind(y = data[, 1], inferences$series)
  if (!is.null(names(data))) {
    names(inferences$series)[1] <- names(data)[1]
  }
  inferences$series <-
    inferences$series[, -which(names(inferences$series) == "y.model")]

  # Return 'CausalImpact' object
  model <- list(pre.period = pre.period,
                post.period = post.period,
                model.args = model.args,
                bsts.model = bsts.model,
                alpha = alpha)
  impact <- list(series = inferences$series,
                 summary = inferences$summary,
                 protocol = inferences$protocol,
                 model = model)
  class(impact) <- "CausalImpact"
  return(impact)
}

# ------------------------------------------------------------------------------
RunWithBstsModel <- function(bsts.model, y.post, alpha = 0.05) {
  # Runs an impact analysis on top of a fitted bsts model.
  #
  # Args:
  #   bsts.model: fitted model, as returned by bsts(), in which the data
  #               during the post-intervention period was set to NA
  #   y.post:     actual observed data during the post-intervention period
  #   alpha:      tail-probabilities of posterior intervals
  #
  # Returns:
  #   See CausalImpact().

  # Guess <pre.period> and <post.period> from the observation vector
  # These will be needed for plotting period boundaries in plot().
  y <- as.vector(bsts.model$original.series)
  indices <- TryStop(InferPeriodIndicesFromData(y),
                     paste0("bsts.model must have been fitted on data where ",
                            "the values in the post-intervention period have ",
                            "been set to NA"))

  # Compile posterior inferences
  inferences <- CompilePosteriorInferences(bsts.model, y.post, alpha)

  # The modeling period comprises everything found in bsts, so the actual
  # observed data is equal to the data in the modeling period
  names(inferences$series)[which(names(inferences$series) == "y.model")] <- "y"

  # Return 'CausalImpact' object
  model <- list(pre.period = indices$pre.period,
                post.period = indices$post.period,
                bsts.model = bsts.model,
                alpha = alpha)
  impact <- list(series = inferences$series,
                 summary = inferences$summary,
                 protocol = inferences$protocol,
                 model = model)
  class(impact) <- "CausalImpact"
  return(impact)
}

# ------------------------------------------------------------------------------
PrintSummary <- function(impact, digits = 2L) {
  # Prints a summary of the results. Both \code{print.CausalImpact()} and
  # \code{summary.CausalImpact()} point here.
  #
  # Args:
  #   impact: A \code{CausalImpact} results object, as returned by
  #           \code{CausalImpact()}.
  #
  #   digits: Number of digits to print for all numbers.

  # Check input
  assert_that(class(impact) == "CausalImpact")
  summary <- impact$summary
  alpha <- impact$model$alpha
  assert(!is.null(alpha) && alpha > 0,
         "invalid <alpha>; <impact> must be a CausalImpact object")

  # Print title
  cat("Posterior inference {CausalImpact}\n")
  if (is.null(summary)) {
    cat("(Inference aborted)\n")
    return()
  }

  # Define formatting helper functions
  StrTrim <- function (x) gsub("^\\s+|\\s+$", "", x)
  FormatNumber <- function(x) StrTrim(format(x, digits = digits))
  FormatPercent <- function(x)
    StrTrim(paste0(format(x * 100, digits = digits), "%"))
  FormatCI <- function(a, b)
    paste0("[", StrTrim(format(a, digits = min(digits, 2))),
           ", ", StrTrim(format(b, digits = min(digits, 2))),
           "]")
  FormatPercentCI <- function(a, b)
    paste0("[", StrTrim(format(a * 100, digits = min(digits, 2))),
           "%, ", StrTrim(format(b * 100, digits = min(digits, 2))),
           "%]")

  # Compile data frame with formatted numbers
  fsummary <- data.frame(
      Actual = FormatNumber(summary$Actual),
      Pred = paste0(FormatNumber(summary$Pred),
                    " (", FormatNumber(summary$Pred.sd), ")"),
      Pred.ci = FormatCI(summary$Pred.lower, summary$Pred.upper),
      Separator1 = c("", ""),
      AbsEffect = paste0(FormatNumber(summary$AbsEffect),
                         " (", FormatNumber(summary$AbsEffect.sd), ")"),
      AbsEffect.ci = FormatCI(summary$AbsEffect.lower, summary$AbsEffect.upper),
      Separator2 = c("", ""),
      RelEffect = paste0(FormatPercent(summary$RelEffect),
                         " (", FormatPercent(summary$RelEffect.sd), ")"),
      RelEffect.ci = FormatPercentCI(summary$RelEffect.lower,
                                     summary$RelEffect.upper))

  # Invert and format as table
  tsummary <- t(fsummary)
  colnames(tsummary) <- c("Average", "Cumulative")
  ci.label <- paste0(round((1 - alpha) * 100), "% CI")
  row.names(tsummary) <- c("Actual", "Prediction (s.d.)", ci.label,
                           " ",
                           "Absolute effect (s.d.)", paste(ci.label, ""),
                           "  ",
                           "Relative effect (s.d.)", paste(ci.label, " "))

  # Print formatted table
  cat("\n")
  print.default(tsummary, print.gap = 3L, quote = FALSE)
  cat("\n")

  # Print overall tail-area probability
  p <- summary$p[1]
  cat(paste0("Posterior tail-area probability p:   ", round(p, 5), "\n"))
  cat(paste0("Posterior prob. of a causal effect:  ",
             round((1 - p) * 100, ifelse(p < 0.01, 5, ifelse(p < 0.05, 3, 0))),
             "%\n"))
  cat("\n")
  cat(paste0("For more details, type: summary(impact, \"protocol\")\n"))
  cat("\n")
}

# ------------------------------------------------------------------------------
PrintProtocol <- function(impact) {
  # Prints a detailed protocol of the individual steps carried out during the
  # analysis.
  #
  # Args:
  #   impact: A \code{CausalImpact} results object, as returned by
  #           \code{CausalImpact()}.

  assert_that(class(impact) == "CausalImpact")
  cat("Analysis protocol {CausalImpact}\n")
  if (is.null(impact$protocol)) {
    cat("(Protocol empty)")
  } else {
    cat(paste(impact$protocol, collapse = " "), "\n")
  }
}

# ------------------------------------------------------------------------------
.summary.CausalImpact <- function(impact,
                                  output = c("summary", "protocol"),
                                  ...) {
  # Helper function for summary.CausalImpact(). The latter must adhere to the
  # S3 generic interface summary(x, ...).
  #
  # Args:
  #   impact: CausalImpact results object
  #   output: "summary" or "protocol"
  #   ...: additional arguments

  output <- tolower(match.arg(output))
  if (output == "summary") {
    PrintSummary(impact, ...)
  } else if (output == "protocol") {
    PrintProtocol(impact)
  }
}

# ------------------------------------------------------------------------------
summary.CausalImpact <- function(object, ...) {
  # S3 method for printing a summary of analysis results.
  #
  # Args:
  #   object: A \code{CausalImpact} results object, as returned by
  #           \code{CausalImpact()}.
  #   ...:    Optional additional arguments, as described below. The first is
  #           \code{output}. You can specify the type of desired output using
  #           \code{summary(x, "summary")} (default) or \code{summary(x,
  #           "protocol")}. Partial matches are allowed. Furthermore,
  #           \code{digits} can be used to customize the precision of the output
  #           generated by summary(impact, "summary").
  #
  # Documentation:
  #   usage: summary(x, output = c("summary", "protocol"), ...)

  .summary.CausalImpact(object, ...)
}

# ------------------------------------------------------------------------------
print.CausalImpact <- function(x, ...) {
  # S3 method for printing a summary of analysis results.
  #
  # Args:
  #   x:   A \code{CausalImpact} results object, as returned by
  #        \code{CausalImpact()}.
  #   ...: Optional additional arguments, as described below. The first is
  #        \code{output}. You can specify the type of desired output using
  #        \code{summary(x, "summary")} (default) or \code{summary(x,
  #        "protocol")}. Partial matches are allowed. Furthermore, \code{digits}
  #        can be used to customize the precision of the output generated by
  #        summary(impact, "summary").
  #
  # Documentation:
  #   usage: print(x, output = c("summary", "protocol"), ...)

  .summary.CausalImpact(x, ...)
}
