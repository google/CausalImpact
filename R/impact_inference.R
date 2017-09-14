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

# Posterior inference for bsts models.
#
# Author: kbrodersen@google.com (Kay Brodersen)

GetPosteriorStateSamples <- function(bsts.model) {
  # Returns a matrix of simulated values from the marginal posterior
  # distribution for the sum of all state variables.
  #
  # Args:
  #   bsts.model: A fitted model returned by \code{bsts()}.
  #
  # Returns:
  #   matrix [number of post-burn-in MCMC samples] x [time points]

  # Get state contributions (e.g., 1000 samples x 2 states x 365 time pts),
  # discarding burn-in samples (=> 900 x 2 x 365)
  burn <- SuggestBurn(0.1, bsts.model)
  assert_that(burn > 0)
  state.contributions <- bsts.model$state.contributions[-(1 : burn), , ,
                                                        drop = FALSE]

  # Sum across states, call it 'state.samples' (=> 900 x 365)
  state.samples <- rowSums(aperm(state.contributions, c(1, 3, 2)), dims = 2)
  return(state.samples)
}

ComputeResponseTrajectories <- function(bsts.model) {
  # Generates trajectories of the response variable. A trajectory is a simulated
  # time series drawn from the posterior predictive distribution over the data.
  # This function differs from GetPosteriorStateSamples(). The latter returns
  # the posterior mean of the response. This function returns the actual value
  # (posterior mean + observation noise).
  #
  # Args:
  #   bsts.model: A model object as returned by \code{bsts()}.
  #
  # Returns:
  #   matrix [number of post-burn-in MCMC samples] x [time points]

  # Get posterior state samples
  state.samples <- GetPosteriorStateSamples(bsts.model)

  # Get observation noise standard deviation samples
  burn <- SuggestBurn(0.1, bsts.model)
  assert_that(burn > 0)
  sigma.obs <- bsts.model$sigma.obs[-(1 : burn)]  # e.g., 900

  # Sample from the posterior predictive density over data
  n.samples <- dim(state.samples)[1]  # e.g., 900 x 365
  obs.noise.samples <- matrix(rnorm(prod(dim(state.samples)), 0, sigma.obs),
                              nrow = n.samples)
  y.samples <- state.samples + obs.noise.samples
  return(y.samples)
}

ComputePointPredictions <- function(y.samples, state.samples, alpha = 0.05) {
  # Summarises a matrix of response trajectory samples (\code{y.samples}) in
  # terms of the mean and an interval of the posterior predictive density over
  # the data.
  #
  # Args:
  #   y.samples:     Matrix of simulated response samples.
  #   state.samples: Matrix of posterior state samples (needed for the mean).
  #   alpha:         The resulting coverage of the posterior intervals will be
  #                  \code{1 - alpha}.
  #
  # Returns:
  #   data frame with 3 columns:
  #     point.pred.mean:  posterior predictive expectation
  #     point.pred.lower: lower limit of a \code{(1 - alpha)*100}% interval
  #     point.pred.upper: upper limit

  # Expectation of data = expectation of state (because noise is centered)
  assert_that(identical(dim(y.samples), dim(state.samples)),
              msg = "inconsistent y.samples, state.samples")
  point.pred.mean <- colMeans(state.samples) # e.g., 365

  # Quantiles of the data = Quantiles of (state + observation noise)
  assert_that(is.scalar(alpha), alpha > 0, alpha < 1)
  prob.lower <- alpha / 2      # e.g., 0.025 when alpha = 0.05
  prob.upper <- 1 - alpha / 2  # e.g., 0.975 when alpha = 0.05
  point.pred.lower <- as.numeric(t(apply(y.samples, 2, quantile, prob.lower)))
  point.pred.upper <- as.numeric(t(apply(y.samples, 2, quantile, prob.upper)))
  point.pred <- data.frame(point.pred = point.pred.mean,
                           point.pred.lower, point.pred.upper)
  return(point.pred)
}

ComputeCumulativePredictions <- function(y.samples, point.pred, y,
                                         post.period.begin, alpha = 0.05) {
  # Computes summary statistics for the cumulative posterior predictions over
  # the unobserved data points in the post-intervention period.
  #
  # Args:
  #   y.samples:         Matrix of simulated response trajectories, as returned
  #                      by \code{ComputeResponseTrajectories()}.
  #   point.pred:        Data frame of point predictions, as returned by
  #                      \code{ComputePointPredictions()}.
  #   y:                 Actual observed response, from the beginning of the
  #                      pre-period to the end of the observed period.
  #   post.period.begin: Index of the first data point of the post-period.
  #   alpha:             The resulting coverage of the posterior intervals will
  #                      be \code{1 - alpha}.
  #
  # Returns:
  #   data frame with 3 columns:
  #     cum.pred:       posterior predictive expectation
  #     cum.pred.lower: lower limit of a \code{(1 - alpha)*100}% interval
  #     cum.pred.upper: upper limit

  # After pre-inference standardization of the response variable has been
  # undone, we can form cumulative time series of counterfactual predictions.
  # Note that we only explicitly compute these for the post-period. The
  # cumulative prediction for the pre-period and the gap between pre- and post-
  # period (if any) is forced to equal the (cumulative) observed response in the
  # pre-period. Thus, the posterior intervals of the cumulative predictions do
  # not inherit variance from the pre-period, which would be misleading when
  # subtracting cumulative predictions from the cumulative observed response to
  # obtain the cumulative impact, which is the main use case. Thus, the
  # cumulative impact will be zero by construction before the beginning of the
  # post-period.

  # Compute posterior mean
  is.post.period <- (1 : length(y)) >= post.period.begin
  cum.pred.mean.pre <- cumsum.na.rm(as.vector(y)[1 : (post.period.begin - 1)])
  non.na.indices <- which(!is.na(cum.pred.mean.pre[1:(post.period.begin - 1)]))
  assert_that(length(non.na.indices) > 0)
  last.non.na.index <- max(non.na.indices)
  cum.pred.mean.post <- cumsum(point.pred$point.pred[is.post.period]) +
      cum.pred.mean.pre[last.non.na.index]
  cum.pred.mean <- c(cum.pred.mean.pre, cum.pred.mean.post)

  # Check for overflow
  assert_that(identical(which(is.na(cum.pred.mean)),
                        which(is.na(y[1:(post.period.begin - 1)]))),
              msg = "unexpected NA found in cum.pred.mean")

  # Compute posterior interval
  cum.pred.lower.pre <- cum.pred.mean.pre
  cum.pred.upper.pre <- cum.pred.mean.pre
  y.samples.cum.post <- t(apply(y.samples[, is.post.period, drop = FALSE], 1,
                                cumsum)) +
      cum.pred.mean.pre[last.non.na.index]
  if (sum(is.post.period) == 1) {
    y.samples.cum.post <- t(y.samples.cum.post)
  }
  assert_that(is.scalar(alpha), alpha > 0, alpha < 1)
  prob.lower <- alpha / 2      # e.g., 0.025 when alpha = 0.05
  prob.upper <- 1 - alpha / 2  # e.g., 0.975 when alpha = 0.05
  cum.pred.lower.post <- as.numeric(t(apply(y.samples.cum.post, 2, quantile,
                                            prob.lower)))
  cum.pred.upper.post <- as.numeric(t(apply(y.samples.cum.post, 2, quantile,
                                            prob.upper)))
  cum.pred.lower <- c(cum.pred.lower.pre, cum.pred.lower.post)
  cum.pred.upper <- c(cum.pred.upper.pre, cum.pred.upper.post)

  # Put cumulative prediction together
  cum.pred <- data.frame(cum.pred = cum.pred.mean,
                         cum.pred.lower, cum.pred.upper)
  return(cum.pred)
}

# Tell R CMD check to treat columns of data frames used in `dplyr::mutate` as
# global variables; this avoids false positives of "no visible binding for
# global variable ..." during the check.
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("AbsEffect", "AbsEffect.lower", "AbsEffect.upper",
                           "AbsEffect.sd", "Pred"))
}

CompileSummaryTable <- function(y.post, y.samples.post,
                                point.pred.mean.post, alpha = 0.05) {
  # Creates a table of statistics that summarise the post-intervention period.
  # This will later be accessible through \code{impact$model$summary}.
  #
  # Args:
  #   y.post:               Actual observed response during the post-period.
  #   y.samples.post:       Matrix of sampled response trajectories for the
  #                         post-period.
  #   point.pred.mean.post: Posterior predictive mean for the post-period. Note
  #                         that colMeans(y.samples.post) = point.pred.mean.post
  #                         in expectation (i.e., in the limit of an infinite
  #                         number of MCMC iterations); but for any given finite
  #                         simulation, y.samples.post contains sampled
  #                         observation noise. Therefore, to obtain a summary of
  #                         the posterior mean series, we consider the mean of
  #                         the posterior predictive level, without additional
  #                         simulated (centered) observation noise.
  #   alpha:                The resulting coverage of the posterior intervals
  #                         will be \code{1 - alpha}.
  #
  # Returns:
  #   data frame of post-period summary statistics

  # Check input
  assert_that(ncol(y.samples.post) == length(y.post),
              msg = "inconsistent y.post")
  assert_that(length(point.pred.mean.post) == length(y.post),
              msg = "inconsistent y.post")

  # We will compare the matrix of predicted trajectories (e.g., 900 x 201)
  # with a matrix of replicated observations (e.g., 900 x 201)
  y.repmat.post <- matrix(y.post, nrow = nrow(y.samples.post),
                          ncol = length(y.post), byrow = TRUE)
  assert_that(all(dim(y.repmat.post) == dim(y.samples.post)))

  # Define quantiles
  assert_that(is.scalar(alpha), alpha > 0, alpha < 1)
  prob.lower <- alpha / 2      # e.g., 0.025 when alpha = 0.05
  prob.upper <- 1 - alpha / 2  # e.g., 0.975 when alpha = 0.05

  # Compile summary statistics
  summary <- data.frame(
      Actual = c(mean(y.post), sum(y.post)),
      Pred = c(mean(point.pred.mean.post), sum(point.pred.mean.post)),
      Pred.lower = c(quantile(rowMeans(y.samples.post), prob.lower),
                     quantile(rowSums(y.samples.post), prob.lower)),
      Pred.upper = c(quantile(rowMeans(y.samples.post), prob.upper),
                     quantile(rowSums(y.samples.post), prob.upper)),
      Pred.sd = c(sd(rowMeans(y.samples.post)),
                  sd(rowSums(y.samples.post))),
      AbsEffect = c(mean(y.post) - mean(point.pred.mean.post),
                    sum(y.post) - sum(point.pred.mean.post)),
      AbsEffect.lower = c(quantile(rowMeans(y.repmat.post - y.samples.post),
                                   prob.lower),
                          quantile(rowSums(y.repmat.post - y.samples.post),
                                   prob.lower)),
      AbsEffect.upper = c(quantile(rowMeans(y.repmat.post - y.samples.post),
                                   prob.upper),
                          quantile(rowSums(y.repmat.post - y.samples.post),
                                   prob.upper)),
      AbsEffect.sd = c(sd(rowMeans(y.repmat.post - y.samples.post)),
                       sd(rowSums(y.repmat.post - y.samples.post))))
  summary <- dplyr::mutate(summary, RelEffect = AbsEffect / Pred,
                           RelEffect.lower = AbsEffect.lower / Pred,
                           RelEffect.upper = AbsEffect.upper / Pred,
                           RelEffect.sd = AbsEffect.sd / Pred)
  rownames(summary) <- c("Average", "Cumulative")

  # Add interval coverage, defined by alpha
  summary$alpha <- alpha

  # Add one-sided tail-area probability of overall impact, p
  y.samples.post.sum <- rowSums(y.samples.post)
  y.post.sum <- sum(y.post)
  p <- min(sum(c(y.samples.post.sum, y.post.sum) >= y.post.sum),
           sum(c(y.samples.post.sum, y.post.sum) <= y.post.sum)) /
      (length(y.samples.post.sum) + 1)
  assert_that(p > 0, p < 1)
  summary$p <- p
  return(summary)
}

InterpretSummaryTable <- function(summary, digits = 2L) {
  # Composes a written interpretation of a given summary table.
  #
  # Args:
  #   summary: Data frame with summary statistics, as created within
  #            FitImpactModel().
  #   digits:  Number of digits to print for all numbers. Note that percentages
  #            are always rounded to whole numbers.
  #
  # Returns:
  #   A string summarizing the summary verbally as one would do in the Results
  #   section of a paper.

  # Prepare formatted numbers
  actual <- PrettifyNumber(summary$Actual, round.digits = digits)
  letter <- IdentifyNumberAbbreviation(actual)
  pred <- PrettifyNumber(summary$Pred, letter, 2)
  pred.lower <- PrettifyNumber(summary$Pred.lower, letter, digits)
  pred.upper <- PrettifyNumber(summary$Pred.upper, letter, digits)
  abs.effect <- PrettifyNumber(summary$AbsEffect, letter, digits)
  abs.effect.lower <- PrettifyNumber(summary$AbsEffect.lower, letter, digits)
  abs.effect.upper <- PrettifyNumber(summary$AbsEffect.upper, letter, digits)
  rel.effect <- PrettifyPercentage(summary$RelEffect)
  rel.effect.lower <- PrettifyPercentage(summary$RelEffect.lower)
  rel.effect.upper <- PrettifyPercentage(summary$RelEffect.upper)

  # Evaluate significance and direction of the effect (increase or decrease)
  sig <- (! ((summary$RelEffect.lower[1] < 0) &&
             (summary$RelEffect.upper[1] > 0)))
  pos <- summary$RelEffect[1] > 0
  p <- summary$p[1]

  # Interval name
  ci.coverage <- paste0(round((1 - summary$alpha[1]) * 100), "%")

  # Initialize statement
  stmt <- NULL

  # Summarize averages
  stmt <- paste0(stmt, "\n\nDuring the post-intervention period, the response ",
                 "variable had an average value of approx. ", actual[1],
                 ". ", if (sig) "By contrast, in " else "In ",
                 "the absence of an intervention, ",
                 "we would have expected an average response of ",
                 pred[1], ". The ", ci.coverage, " interval of this ",
                 "counterfactual prediction is [", pred.lower[1],
                 ", ", pred.upper[1], "]. Subtracting this ",
                 "prediction from the observed response yields an estimate ",
                 "of the causal effect the intervention had on the response ",
                 "variable. This effect is ", abs.effect[1], " with a ",
                 ci.coverage, " interval of [", abs.effect.lower[1], ", ",
                 abs.effect.upper[1], "]. For a discussion of ",
                 "the significance of this effect, see below.")

  # Summarize sums
  stmt <- paste0(stmt, "\n\nSumming up the individual data points during ",
                 "the post-intervention period (which can only sometimes be ",
                 "meaningfully interpreted), the response variable had an ",
                 "overall value of ", actual[2], ". ",
                 if (sig) "By contrast, had " else "Had ",
                 "the intervention not taken place, we would have expected ",
                 "a sum of ", pred[2], ". The ", ci.coverage, " interval of ",
                 "this prediction is [", pred.lower[2], ", ", pred.upper[2],
                 "].")

  # Summarize relative numbers (in which case row [1] = row [2])
  stmt <- paste0(stmt, "\n\nThe above results are given in terms of ",
                 "absolute numbers. In relative terms, the response variable ",
                 "showed ", if (pos) "an increase of " else "a decrease of",
                 rel.effect[1], ". The ", ci.coverage, " interval of this ",
                 "percentage is [", rel.effect.lower[1], ", ",
                 rel.effect.upper[1], "].")

  # Comment on significance
  if (sig && pos) {
    stmt <- paste0(stmt, "\n\nThis means that the positive effect observed ",
                   "during the intervention period is statistically ",
                   "significant and unlikely to be due to random ",
                   "fluctuations. ",
                   "It should be noted, however, that the question of whether ",
                   "this increase also bears substantive significance can ",
                   "only be answered by comparing the absolute effect (",
                   abs.effect[1], ") to the original goal of ",
                   "the underlying intervention.")
  } else if (sig && !pos) {
    stmt <- paste0(stmt, "\n\nThis means that the negative effect observed ",
                   "during the intervention period is statistically ",
                   "significant. If the experimenter had expected a positive ",
                   "effect, it is recommended to ",
                   "double-check whether anomalies in the control variables ",
                   "may have caused an overly optimistic expectation of ",
                   "what should have happened in the response variable in the ",
                   "absence of the intervention.")
  } else if (!sig && pos) {
    stmt <- paste0(stmt, "\n\nThis means that, although the intervention ",
                   "appears to have caused a positive effect, this effect ",
                   "is not statistically significant when considering the ",
                   "entire post-intervention period as a whole. Individual ",
                   "days or shorter stretches within the intervention period ",
                   "may of course still have had a significant effect, as ",
                   "indicated whenever the lower limit of the impact ",
                   "time series (lower plot) was above zero.")
  } else if (!sig && !pos) {
    stmt <- paste0(stmt, "\n\nThis means that, although it may look as ",
                   "though the intervention has exerted a negative effect ",
                   "on the response variable when considering the ",
                   "intervention period as a whole, this effect is not ",
                   "statistically significant, and so cannot be ",
                   "meaningfully interpreted.")
  }
  if (!sig) {
    stmt <- paste0(stmt, " The apparent effect could be the result of ",
                   "random fluctuations that are unrelated to the ",
                   "intervention. This is often the case when the ",
                   "intervention period is very long and includes much ",
                   "of the time when the effect has already worn off. ",
                   "It can also be the case when the intervention period ",
                   "is too short to distinguish the signal from the noise. ",
                   "Finally, failing to find a significant effect can ",
                   "happen when there are not enough control variables or ",
                   "when these variables do not correlate well with ",
                   "the response variable during the learning period.")
  }
  if (p < summary$alpha[1]) {
    stmt <- paste0(stmt, "\n\nThe probability of obtaining this effect by ",
                   "chance is very small (Bayesian one-sided tail-area ",
                   "probability p = ", round(p, 3), "). This means the causal ",
                   "effect can be considered statistically significant.")
  } else {
    stmt <- paste0(stmt, "\n\nThe probability of obtaining this ",
                   "effect by chance is p = ", round(p, 3), ". This ",
                   "means the effect may be spurious and would generally ",
                   "not be considered statistically significant.")
  }
  return(stmt)
}

AssertCumulativePredictionsAreConsistent <- function(cum.pred, post.period,
                                                     summary) {
  # Asserts that <cum.pred> is consistent with <summary>.
  #
  # Args:
  #   cum.pred:          Data frame of: cum.pred, cum.pred.lower, cum.upper.
  #   post.period:       A vector of two indices specifying the first and the
  #                      last time point of the post-intervention period.
  #   summary:           Summary table, as created by
  #                      \code{CompileSummaryTable()}.

  # Auxiliary function checking if one column of `cum.pred` is consistent with
  # the corresponding number in `summary`.
  AssertCumulativePredictionIsConsistent <- function(cum.pred.col,
                                                     summary.entry,
                                                     description) {
    non.na.indices <- which(!is.na(cum.pred.col[1:(post.period[1] - 1)]))
    assert_that(length(non.na.indices) > 0)
    last.non.na.index <- max(non.na.indices)
    assert_that(
        is.numerically.equal(cum.pred.col[post.period[2]] -
                               cum.pred.col[last.non.na.index],
                             summary.entry[2]),
        msg = paste0("The calculated ", description, " of the cumulative ",
                     "effect is inconsistent with the previously calculated ",
                     "one. You might try to run CausalImpact on a shorter ",
                     "time series to avoid this problem."))
  }

  AssertCumulativePredictionIsConsistent(cum.pred$cum.pred, summary$Pred,
                                         "mean")
  AssertCumulativePredictionIsConsistent(cum.pred$cum.pred.lower,
                                         summary$Pred.lower, "lower bound")
  AssertCumulativePredictionIsConsistent(cum.pred$cum.pred.upper,
                                         summary$Pred.upper, "upper bound")
}

CheckInputForCompilePosteriorInferences <- function(bsts.model, y.cf,
                                                    post.period, alpha,
                                                    UnStandardize) {
  # Checks the input arguments for CompilePosteriorInferences().
  #
  # Args:
  #   bsts.model:    Model object created by bsts().
  #   y.cf:          Actual observed data in the counterfactual period, i.e.
  #                  after pre-intervention period (vector or zoo object).
  #   post.period:   A vector of two indices specifying the first and the last
  #                  time point of the post-intervention period.
  #   alpha:         Level for credible intervals.
  #   UnStandardize: Function for undoing any data standardization.
  #
  # Returns:
  #   list of checked arguments

  # Check <bsts.model>
  assert_that(!is.null(bsts.model))
  assert_that(class(bsts.model) == "bsts")
  assert_that(length(bsts.model$original.series) >= 2)

  # Check <post.period>
  assert_that(is.vector(post.period))
  assert_that(is.numeric(post.period))
  assert_that(length(post.period) == 2)
  assert_that(!anyNA(post.period))
  # Check that <post.period> lies within the range covered by <y.cf>
  cf.period.start <- length(bsts.model$original.series) - length(y.cf) + 1
  assert_that(cf.period.start <= post.period[1])
  assert_that(post.period[1] <= post.period[2])
  assert_that(post.period[2] <= length(bsts.model$original.series))

  # Check <y.cf>
  assert_that(is.zoo(y.cf) || is.vector(y.cf))
  y.cf <- as.vector(y.cf)
  assert_that(is.numeric(y.cf))
  assert_that(length(y.cf) >= 1)
  assert_that(!anyNA(y.cf[(post.period[1] : post.period[2]) -
                            cf.period.start + 1]),
              msg = "NA values in the post-period not currently supported")
  assert_that(all(is.na(tail(bsts.model$original.series, length(y.cf)))),
              msg = paste0("bsts.model$original.series must end on a stretch ",
                           "of NA at least as long as y.cf"))

  # Check <alpha>
  assert_that(is.numeric(alpha))
  assert_that(is.scalar(alpha))
  assert_that(!is.na(alpha))
  assert_that(alpha > 0, alpha < 1)

  # Check <UnStandardize>
  assert_that(is.function(UnStandardize))
  assert_that(is.scalar(UnStandardize(1)))
  assert_that(is.numeric(UnStandardize(1)))
  assert_that(length(UnStandardize(c(1, 2))) == 2)

  # Return arguments
  return(list(bsts.model = bsts.model,
              y.cf = y.cf,
              post.period = post.period,
              alpha = alpha,
              UnStandardize = UnStandardize))
}

CompilePosteriorInferences <- function(bsts.model, y.cf, post.period,
                                       alpha = 0.05, UnStandardize = identity) {
  # Takes in a fitted \code{bsts} model and computes the posterior predictive
  # distributions, over time, for the counterfactual response and the causal
  # effect.
  #
  # Args:
  #   bsts.model:    A model object created by \code{bsts()}.
  #   y.cf:          Actual observed data in the counterfactual period, i.e.
  #                  after pre-intervention period (vector or zoo object).
  #   post.period:   A vector of two indices specifying the first and the last
  #                  time point of the post-intervention period.
  #   alpha:         The resulting coverage of the posterior intervals will be
  #                  \code{1 - alpha}.
  #   UnStandardize: If \code{bsts()} was run on standardized data, this is the
  #                  function to undo that standardization. This is critical for
  #                  obtaining correct cumulative predictions.
  #
  # Returns:
  #   series:  zoo time-series object of: point.pred, point.pred.lower, ...
  #   summary: table of summary statistics
  #   report:  verbal description of the summary statistics

  # Check input
  checked <- CheckInputForCompilePosteriorInferences(bsts.model, y.cf,
                                                     post.period, alpha,
                                                     UnStandardize)
  bsts.model <- checked$bsts.model
  y.cf <- checked$y.cf
  post.period <- checked$post.period
  alpha <- checked$alpha
  UnStandardize <- checked$UnStandardize

  # Compute point predictions of counterfactual (in standardized space)
  y.samples <- ComputeResponseTrajectories(bsts.model)
  state.samples <- GetPosteriorStateSamples(bsts.model)
  point.pred <- ComputePointPredictions(y.samples, state.samples, alpha)

  # Undo standardization (if any)
  y.samples <- UnStandardize(y.samples)
  point.pred <- UnStandardize(point.pred)
  y.model <- UnStandardize(bsts.model$original.series)

  # Reconstruct full original series
  indices <- seq_along(y.model)
  is.cf.period <- (indices >= length(y.model) - length(y.cf) + 1)
  y.model[is.cf.period] <- y.cf

  # Compile summary statistics (in original space). Summary statistics consider
  # quantities in the post-period only, not in the whole counterfactual period.
  is.post.period <- (indices >= post.period[1]) & (indices <= post.period[2])
  y.samples.post <- y.samples[, is.post.period, drop = FALSE]
  point.pred.mean.post <- point.pred$point.pred[is.post.period]
  y.post <- y.cf[tail(is.post.period, length(y.cf))]
  summary <- CompileSummaryTable(y.post, y.samples.post, point.pred.mean.post,
                                 alpha)
  report <- InterpretSummaryTable(summary)

  # Compute cumulative predictions (in original space)
  cum.pred <- ComputeCumulativePredictions(y.samples, point.pred, y.model,
                                           post.period[1], alpha)

  # Check that <cum.pred> is consistent with <summary>
  AssertCumulativePredictionsAreConsistent(cum.pred, post.period, summary)

  # Create results series
  cum.y.model <- cumsum.na.rm(y.model)
  series <- zoo(data.frame(y.model, cum.y.model, point.pred, cum.pred),
                time(y.model))
  series$point.effect <- series$y.model - series$point.pred
  series$point.effect.lower <- series$y.model - series$point.pred.upper
  series$point.effect.upper <- series$y.model - series$point.pred.lower
  series$cum.effect <- series$cum.y.model - series$cum.pred
  series$cum.effect.lower <- series$cum.y.model - series$cum.pred.upper
  series$cum.effect.upper <- series$cum.y.model - series$cum.pred.lower
  assert_that(nrow(series) == length(bsts.model$original.series))

  # Set effects and cumulative effects to NA at time points not belonging to
  # pre- or post-period.
  # Note that since the time series is cut to the beginning of the pre-period
  # before being given to CompilePosteriorInferences, pre-period and
  # non-counterfactual period are identical here.
  effect.cols <- grep("(point|cum)\\.effect", names(series))
  series[is.cf.period & !is.post.period, effect.cols] <- NA

  # Return <series> and <summary>
  return(list(series = series,
              summary = summary,
              report = report))
}

CompileNaInferences <- function(y.model) {
  # Creates a data frame of inferences that are all NA. We do this when the
  # response data were ill-conditioned (e.g., all constant).
  #
  # Args:
  #   y.model: actual observed response in the modeling period (zoo object)

  # Check input
  assert_that(is.zoo(y.model))
  assert_that(length(y.model) >= 1)

  # Create NA inferences
  vars <- c("point.pred", "point.pred.lower", "point.pred.upper",
            "cum.pred", "cum.pred.lower", "cum.pred.upper",
            "point.effect", "point.effect.lower", "point.effect.upper",
            "cum.effect", "cum.effect.lower", "cum.effect.upper")
  na.series <- matrix(as.numeric(NA), nrow = length(y.model), ncol = 12)
  na.series <- zoo(na.series, time(y.model))
  names(na.series) <- vars

  # Insert observed data, as we do in CompilePosteriorInferences()
  cum.y.model <- cumsum(y.model)
  series <- zoo(cbind(y.model = y.model, cum.y.model = cum.y.model, na.series),
                time(y.model))

  # Return NA <series> and NULL <summary>
  return(list(series = series,
              summary = NULL,
              report = NULL))
}
