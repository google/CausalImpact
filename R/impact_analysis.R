# Copyright 2014-2017 Google Inc. All rights reserved.
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

FormatInputData <- function(data) {
  # Checks and formats the <data> argument provided to CausalImpact().
  #
  # Args:
  #   data: a zoo object, a vector, a matrix, or a data frame.
  #
  # Returns:
  #   correctly formatted zoo object

  # Check if `data` is a valid data type: a zoo object, a numerical vector, a
  # matrix, or a data frame.
  assert_that(is.zoo(data) || is.data.frame(data) ||
              ((is.vector(data) || is.matrix(data)) && is.numeric(data)))

  # If `data` is a data frame and the first column is 'date', try to convert.
  if (is.data.frame(data) && tolower(names(data)[1]) %in% c("date", "time")) {
    if (class(data$date) == "Date") {
      data <- zoo(data[, -1], data$date)
    } else {
      warning(paste0("Did you mean: data = zoo(data[, -1], data$",
                     names(data)[1], ")"))
    }
  }

  # Try to convert to zoo object, and assert that data is numeric.
  data <- TryStop(as.zoo(data), "could not convert input data to zoo object")
  assert_that(is.numeric(data))

  # Ensure <data> is formatted in such a way that rows represent time points
  if (is.null(ncol(data))) {
    dim(data) <- c(length(data), 1)
  }

  # Must have at least 3 time points
  assert_that(nrow(data) > 3)

  # Must not have NA in covariates (if any)
  if (ncol(data) >= 2) {
    assert_that(!anyNA(data[, -1]))
  }

  # Convert data from integer to double if necessary; this avoids overflow
  # problems if data values are large (i.e., close to the maximum range of
  # integer values).
  if (is.integer(data)) {
    data.matrix <- coredata(data)
    coredata(data) <- matrix(as.numeric(data.matrix), nrow = nrow(data.matrix),
                             ncol = ncol(data.matrix),
                             dimnames = dimnames(data.matrix))
  }

  return(data)
}

FormatInputPrePostPeriod <- function(pre.period, post.period, data) {
  # Checks `pre.period` and `post.period` input arguments, and returns the
  # corresponding time series indices.
  #
  # Args:
  #   pre.period:  two-element vector of pre-period boundaries in the time unit
  #                of `time(data)`.
  #   post.period: two-element vector of post-period boundaries in the time unit
  #                of `time(data)`.
  #   data:        already-checked zoo object, for reference only.
  #
  # Returns:
  #   List with entries `pre.period` and `post.period`, containing indices of
  #   period boundaries (relative to `time(data)`).

  assert_that(!is.null(pre.period))
  assert_that(!is.null(post.period))
  assert_that(length(pre.period) == 2, length(post.period) == 2)
  assert_that(!anyNA(pre.period), !anyNA(post.period))
  assert_that(isTRUE(all.equal(class(time(data)), class(pre.period))) ||
                (is.numeric(time(data)) && is.numeric(pre.period)),
              msg = paste0("pre.period (", class(pre.period)[1], ") ",
                           "must have the same class as the time points in ",
                           "the data (", class(time(data))[1], ")"))
  assert_that(isTRUE(all.equal(class(time(data)), class(post.period))) ||
                (is.numeric(time(data)) && is.numeric(post.period)),
              msg = paste0("post.period (", class(post.period)[1], ") ",
                           "must have the same class as the time points in ",
                           "the data (", class(time(data))[1], ")"))
  if (pre.period[1] < start(data)) {
    warning(paste0("Setting pre.period[1] to start of data: ", start(data)))
  }
  if (pre.period[2] > end(data)) {
    warning(paste0("Setting pre.period[2] to end of data: ", end(data)))
  }
  if (post.period[2] > end(data)) {
    warning(paste0("Setting post.period[2] to end of data: ", end(data)))
  }

  period.indices <- list(
      pre.period = GetPeriodIndices(pre.period, time(data)),
      post.period = GetPeriodIndices(post.period, time(data)))
  assert_that(diff(period.indices$pre.period) >= 2,
              msg = "pre.period must span at least 3 time points")
  assert_that(period.indices$post.period[1] > period.indices$pre.period[2])

  return(period.indices)
}

FormatInputForCausalImpact <- function(data, pre.period, post.period,
                                       model.args, bsts.model,
                                       post.period.response, alpha) {
  # Checks and formats all input arguments supplied to CausalImpact(). See the
  # documentation of CausalImpact() for details.
  #
  # Args:
  #   data:                 zoo object or data frame
  #   pre.period:           beginning and end of pre-period
  #   post.period:          beginning and end of post-period
  #   model.args:           list of additional arguments for the model
  #   bsts.model:           fitted bsts model (instead of data)
  #   post.period.response: observed response in the post-period
  #   alpha:                tail-area for posterior intervals
  #
  # Returns:
  #   list of checked (and possibly reformatted) input arguments

  # Check that a consistent set of variables has been provided
  assert_that(
      xor(!is.null(data) && !is.null(pre.period) && !is.null(post.period) &&
            is.null(bsts.model) && is.null(post.period.response),
          is.null(data) && is.null(pre.period) && is.null(post.period) &&
            !is.null(bsts.model) && !is.null(post.period.response)),
      msg = paste0("must either provide data, pre.period, post.period, ",
                   "model.args; or bsts.model and post.period.response"))

  # Check <data> and convert to zoo, with rows representing time points
  if (!is.null(data)) {
    data <- FormatInputData(data)
  }

  # Check `pre.period` and `post.period`, and convert them to period indices.
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
  assert_that(is.scalar(model.args$standardize.data))
  assert_that(is.logical(model.args$standardize.data))
  assert_that(!is.na(model.args$standardize.data))

  # Check <bsts.model>
  if (!is.null(bsts.model)) {
    assert_that(class(bsts.model) == "bsts")
  }

  # Check <post.period.response>
  if (!is.null(bsts.model)) {
    assert_that(!is.null(post.period.response),
                is.vector(post.period.response),
                is.numeric(post.period.response))
  }

  # Check <alpha>
  assert_that(is.numeric(alpha))
  assert_that(is.scalar(alpha))
  assert_that(!is.na(alpha))
  assert_that(alpha > 0, alpha < 1)

  # Return updated arguments
  return(list(data = data, pre.period = pre.period, post.period = post.period,
              model.args = model.args, bsts.model = bsts.model,
              post.period.response = post.period.response, alpha = alpha))
}

CausalImpact <- function(data = NULL,
                         pre.period = NULL,
                         post.period = NULL,
                         model.args = NULL,
                         bsts.model = NULL,
                         post.period.response = NULL,
                         alpha = 0.05) {
  # CausalImpact() performs causal inference through counterfactual
  # predictions using a Bayesian structural time-series model.
  #
  # Detailed and up-to-date documentation is provided in
  # ../man/CausalImpact.Rd. Type ?CausalImpact to display the documentation.
  # For example code, see the package vignette
  # (http://google.github.io/CausalImpact/).
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
  #   pre.period:  A vector specifying the first and the last time point of the
  #                pre-intervention period in the response vector \code{y}. This
  #                period can be thought of as a training period, used to
  #                determine the relationship between the response variable and
  #                the covariates. If \code{data} is a \code{zoo} object with
  #                a \code{time} attribute, \code{pre.period} must be indicated
  #                using the same time scale (i.e. using the same class as
  #                \code{time(data)}, see examples). If \code{data} doesn't have
  #                a \code{time} attribute, \code{post.period} is indicated with
  #                indices.
  #
  #   post.period: A vector specifying the first and the last day of the
  #                post-intervention period we wish to study. This is the period
  #                after the intervention has begun whose effect we are
  #                interested in. The relationship between response variable and
  #                covariates, as determined during the pre-period, will be used
  #                to predict how the response variable should have evolved
  #                during the post-period had no intervention taken place. If
  #                \code{data} is a \code{zoo} object with a \code{time}
  #                attribute, \code{post.period} must be indicated using the
  #                same time scale. If \code{data} doesn't have a \code{time}
  #                attribute, \code{post.period} is indicated with indices.
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
  #   post.period.response: Actual observed data during the post-intervention
  #                period. This is required if and only if a fitted
  #                \code{bsts.model} is passed instead of \code{data}.
  #
  #   alpha:       Desired tail-area probability for posterior intervals.
  #                Defaults to 0.05, which will produce central 95\% intervals.
  #
  # Returns:
  #   A CausalImpact object. This is a list of:
  #     series:  observed data, counterfactual, pointwise and cumulative impact
  #     summary: summary table
  #     report:  verbal description of the analysis
  #     model:   list with four elements \code{pre.period}, \code{post.period},
  #              \code{bsts.model} and \code{alpha}. \code{pre.period} and
  #              \code{post.period} indicate the first and last time point of
  #              the time series in the respective period, \code{bsts.model} is
  #              the fitted model returned by \code{bsts()}, and \code{alpha}
  #              is the user-specified tail-area probability.
  #
  # Optional arguments for model.args:
  #   niter:              number of MCMC iterations
  #   standardize.data:   whether to standardize the data over the
  #                       pre-intervention period before model fitting
  #   prior.level.sd:     standard deviation of the prior on the local level
  #   nseasons:           number of seasons in the seasonal component
  #   season.duration:    duration of each season
  #   dynamic.regression: whether to have dynamic instead of static coefficients
  #
  # For more details on all of the above, see the package manual (?CausalImpact)
  # or the vignette.
  #
  # Examples:
  #   # Time series without dates:
  #   set.seed(1)
  #   x1 <- 100 + arima.sim(model = list(ar = 0.999), n = 100)
  #   y <- 1.2 * x1 + rnorm(100)
  #   y[71:100] <- y[71:100] + 10
  #   data <- cbind(y, x1)
  #   pre.period <- c(1, 70)
  #   post.period <- c(71, 100)
  #
  #   impact <- CausalImpact(data, pre.period, post.period)
  #
  #   summary(impact)
  #   summary(impact, "report")
  #   plot(impact)
  #
  #   # Daily time series:
  #   times <- seq.Date(as.Date("2015-01-01"), by = 1, length.out = 100)
  #   data <- zoo(cbind(y, x1), times)
  #
  #   impact <- CausalImpact(data, times[pre.period], times[post.period])
  #
  #   summary(impact)
  #   summary(impact, "report")
  #   plot(impact)
  #
  #   # Analysis based on a `bsts` model:
  #   post.period.response <- y[post.period[1] : post.period[2]]
  #   post.period.response[post.period[1] : post.period[2]] <- NA
  #   ss <- AddLocalLevel(list(), y)
  #   bsts.model <- bsts(y ~ x1, ss, niter = 1000)
  #   impact <- CausalImpact(bsts.model = bsts.model,
  #                          post.period.response = post.period.response)

  # Check input
  checked <- FormatInputForCausalImpact(data, pre.period, post.period,
                                        model.args, bsts.model,
                                        post.period.response, alpha)
  data <- checked$data
  pre.period <- checked$pre.period
  post.period <- checked$post.period
  model.args <- checked$model.args
  bsts.model <- checked$bsts.model
  post.period.response <- checked$post.period.response
  alpha <- checked$alpha

  # Depending on input, dispatch to the appropriate Run* method()
  if (!is.null(data)) {
    impact <- RunWithData(data, pre.period, post.period, model.args, alpha)
    # Return pre- and post-period in the time unit of the time series.
    times <- time(data)
    impact$model$pre.period <- times[pre.period]
    impact$model$post.period <- times[post.period]
  } else {
    impact <- RunWithBstsModel(bsts.model, post.period.response, alpha)
  }

  return(impact)
}

RunWithData <- function(data, pre.period, post.period, model.args, alpha) {
  # Runs an impact analysis on top of a fitted bsts model.
  #
  # Args:
  #   data:        zoo object of response variable and covariates
  #   pre.period:  two-element vector specifying the indices of the  pre-period
  #                limits.
  #   post.period: two-element vector specifying the indices of the post-period
  #                limits.
  #   model.args:  list of model arguments
  #   alpha:       tail-probabilities of posterior intervals
  #
  # Returns:
  #   See CausalImpact().

  times <- time(data)

  # Zoom in on data in modeling range.
  pre.period[1] <- max(pre.period[1], which.max(!is.na(data[, 1])))
  data.modeling <- window(data, start = times[pre.period[1]])
  if (is.null(ncol(data.modeling))) {
    dim(data.modeling) <- c(length(data.modeling), 1)
  }

  # Standardize all variables?
  UnStandardize <- identity
  if (model.args$standardize.data) {
    fit.range <- c(1, diff(pre.period) + 1)
    sd.results <- StandardizeAllVariables(data.modeling, fit.range)
    data.modeling <- sd.results$data
    UnStandardize <- sd.results$UnStandardize
  }

  # Set observed response after pre-period to NA.
  window(data.modeling[, 1], start = times[pre.period[2] + 1]) <- NA

  # Construct model and perform inference
  bsts.model <- ConstructModel(data.modeling, model.args)

  # Compile posterior inferences
  if (!is.null(bsts.model)) {
    y.cf <- window(data[, 1], start = times[pre.period[2] + 1])
    # We need to adapt post-period indices for `CompilePosteriorInferences()` to
    # specify start and end of the post-period relative to pre.period[1], not
    # relative to the start of the time series; `CompilePosteriorInferences()`
    # only sees the data from the beginning of the pre-period.
    inferences <- CompilePosteriorInferences(bsts.model, y.cf,
                                             post.period - pre.period[1] + 1,
                                             alpha, UnStandardize)
  } else {
    inferences <- CompileNaInferences(data[, 1])
  }

  # Extend <series> to cover original range (padding with NA as necessary)
  empty <- zoo(, times)
  inferences$series <- merge(inferences$series, empty, all = TRUE)
  assert_that(nrow(inferences$series) == nrow(data))

  # Replace <y.model> by full original response
  inferences$series[, 1] <- data[, 1]

  # Assign response-variable names
  names(inferences$series)[1] <- "response"
  names(inferences$series)[2] <- "cum.response"

  # Return 'CausalImpact' object
  model <- list(pre.period = times[pre.period],
                post.period = times[post.period],
                model.args = model.args,
                bsts.model = bsts.model,
                alpha = alpha)
  impact <- list(series = inferences$series,
                 summary = inferences$summary,
                 report = inferences$report,
                 model = model)
  class(impact) <- "CausalImpact"
  return(impact)
}

RunWithBstsModel <- function(bsts.model, post.period.response, alpha = 0.05) {
  # Runs an impact analysis on top of a fitted bsts model.
  #
  # Args:
  #   bsts.model:           fitted model, as returned by bsts(), in which the
  #                         data during the post-period was set to NA
  #   post.period.response: observed data during the post-intervention period
  #   alpha:                tail-probabilities of posterior intervals
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
  if (is.integer(time(bsts.model$original.series))) {
    indices <- lapply(indices, as.integer)
  }

  # Compile posterior inferences
  inferences <- CompilePosteriorInferences(bsts.model = bsts.model,
                                           y.cf = post.period.response,
                                           post.period = indices$post.period,
                                           alpha = alpha)

  # Assign response-variable names
  # N.B. The modeling period comprises everything found in bsts, so the actual
  # observed data is equal to the data in the modeling period
  names(inferences$series)[1] <- "response"
  names(inferences$series)[2] <- "cum.response"

  # Return 'CausalImpact' object
  times <- time(bsts.model$original.series)
  model <- list(pre.period = times[indices$pre.period],
                post.period = times[indices$post.period],
                bsts.model = bsts.model,
                alpha = alpha)
  impact <- list(series = inferences$series,
                 summary = inferences$summary,
                 report = inferences$report,
                 model = model)
  class(impact) <- "CausalImpact"
  return(impact)
}

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
  assert_that(!is.null(alpha) && alpha > 0,
              msg = "invalid <alpha>; <impact> must be a CausalImpact object")

  # Print title
  cat("Posterior inference {CausalImpact}\n")
  if (is.null(summary)) {
    cat("(Inference aborted)\n")
    return()
  }

  # Define formatting helper functions
  StrTrim <- function (x) gsub("^\\s+|\\s+$", "", x)
  FormatNumber <- function(x) StrTrim(format(x, digits = digits))
  FormatPercent <- function(x) {
    StrTrim(paste0(format(x * 100, digits = digits), "%"))
  }
  FormatCI <- function(a, b) {
    paste0("[", StrTrim(format(a, digits = min(digits, 2))),
           ", ", StrTrim(format(b, digits = min(digits, 2))),
           "]")
  }
  FormatPercentCI <- function(a, b) {
    paste0("[", StrTrim(format(a * 100, digits = min(digits, 2))),
           "%, ", StrTrim(format(b * 100, digits = min(digits, 2))),
           "%]")
  }

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
  cat(paste0("For more details, type: summary(impact, \"report\")\n"))
  cat("\n")
}

PrintReport <- function(impact, digits = 2L) {
  # Prints a detailed report of the individual steps carried out during the
  # analysis.
  #
  # Args:
  #   impact: A \code{CausalImpact} results object, as returned by
  #           \code{CausalImpact()}.
  #   digits: Number of digits to print for all numbers. Note that percentages
  #           are always rounded to whole numbers.

  assert_that(class(impact) == "CausalImpact")
  cat("Analysis report {CausalImpact}\n")
  if (is.null(impact$report)) {
    cat("(Report empty)")
  } else {
    cat(paste(InterpretSummaryTable(impact$summary, digits), collapse = " "),
        "\n")
  }
}

.summary.CausalImpact <- function(impact,
                                  output = c("summary", "report"),
                                  ...) {
  # Helper function for summary.CausalImpact(). The latter must adhere to the
  # S3 generic interface summary(x, ...).
  #
  # Args:
  #   impact: CausalImpact results object
  #   output: "summary" or "report"
  #   ...: additional arguments

  output <- tolower(match.arg(output))
  if (output == "summary") {
    PrintSummary(impact, ...)
  } else if (output == "report") {
    PrintReport(impact, ...)
  }
}

summary.CausalImpact <- function(object, ...) {
  # S3 method for printing a summary of analysis results.
  #
  # Args:
  #   object: A \code{CausalImpact} results object, as returned by
  #           \code{CausalImpact()}.
  #   ...:    Optional additional arguments, as described below. The first is
  #           \code{output}. You can specify the type of desired output using
  #           \code{summary(x, "summary")} (default) or \code{summary(x,
  #           "report")}. Partial matches are allowed. Furthermore,
  #           \code{digits} can be used to customize the precision of the
  #           output.
  #
  # Documentation:
  #   usage: summary(x, output = c("summary", "report"), ...)

  .summary.CausalImpact(object, ...)
}

print.CausalImpact <- function(x, ...) {
  # S3 method for printing a summary of analysis results.
  #
  # Args:
  #   x:   A \code{CausalImpact} results object, as returned by
  #        \code{CausalImpact()}.
  #   ...: Optional additional arguments, as described below. The first is
  #        \code{output}. You can specify the type of desired output using
  #        \code{summary(x, "summary")} (default) or \code{summary(x,
  #        "report")}. Partial matches are allowed. Furthermore, \code{digits}
  #        can be used to customize the precision of the output, e.g.:
  #        summary(impact, "summary", digits = 2).
  #
  # Documentation:
  #   usage: print(x, output = c("summary", "report"), ...)

  .summary.CausalImpact(x, ...)
}

as.CausalImpact <- function(x, ...) {
  # S3 method for allowing other packages to write a \code{as.CausalImpact.foo}
  # function that coerces an object of class \code{foo} into a
  # \code{CausalImpact} object.
  #
  # Args:
  #   x:   Any \code{R} object.
  #   ...: Additional arguments to be passed to the method.

  UseMethod("as.CausalImpact")
}

as.CausalImpact.default <- function(x, ...) {
  # Default method for \code{as.CausalImpact}.
  #
  # Args:
  #   x:   Any \code{R} object.
  #   ...: Additional arguments to be passed to the method.

  stop("No method available to coerce an object of class ", class(x)[1],
       " to CausalImpact")
}
