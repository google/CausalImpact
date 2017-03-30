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

testthat::context("Unit tests for impact_inference.R")

# Author: kbrodersen@google.com (Kay Brodersen)

test_that("GetPosteriorStateSamples", {
  GetPosteriorStateSamples <- CausalImpact:::GetPosteriorStateSamples
  ConstructModel <- CausalImpact:::ConstructModel

  # Test empty input
  expect_error(GetPosteriorStateSamples())

  # Generate some input
  data <- zoo(cbind(rnorm(365), rnorm(365), rnorm(365)))
  model.args <- list(niter = 100)

  # Create a healthy bsts.model and test it
  bsts.model <- ConstructModel(data, model.args)
  state.samples <- GetPosteriorStateSamples(bsts.model)
  expect_equal(ncol(state.samples), 365)
})

test_that("ComputeResponseTrajectories", {
  ComputeResponseTrajectories <- CausalImpact:::ComputeResponseTrajectories
  ConstructModel <- CausalImpact:::ConstructModel

  # Test empty input
  expect_error(ComputeResponseTrajectories())

  # Test healthy input
  data <- zoo(cbind(rnorm(365), rnorm(365), rnorm(365)))
  model.args <- list(niter = 100)
  bsts.model <- ConstructModel(data, model.args)
  y.samples <- ComputeResponseTrajectories(bsts.model)
  expect_equal(ncol(y.samples), 365)
})

test_that("ComputePointPredictions", {
  ComputePointPredictions <- CausalImpact:::ComputePointPredictions

  # Test empty input
  expect_error(ComputePointPredictions())

  # Test healthy input
  y.samples <- matrix(rnorm(1000), nrow = 10)
  state.samples <- matrix(rnorm(1000), nrow = 10)
  point.pred <- ComputePointPredictions(y.samples, state.samples, 0.05)
  expect_equal(dim(point.pred), c(100, 3))
  expect_equal(names(point.pred), c("point.pred", "point.pred.lower",
                                    "point.pred.upper"))
})

test_that("ComputeCumulativePredictions", {
  ComputeCumulativePredictions <- CausalImpact:::ComputeCumulativePredictions

  # Test empty input
  expect_error(ComputeCumulativePredictions(), "missing")

  # Test some healthy input
  y.samples <- matrix(rnorm(1000), nrow = 10)
  point.pred <- data.frame(point.pred = rnorm(100),
                           point.pred.lower = rnorm(100),
                           point.pred.upper = rnorm(100))
  y <- rnorm(100)
  cum.pred <- ComputeCumulativePredictions(y.samples, point.pred, y,
                                           post.period.begin = 51,
                                           alpha = 0.05)
  expect_equal(dim(cum.pred), c(100, 3))
  expect_equal(names(cum.pred), c("cum.pred", "cum.pred.lower",
                                  "cum.pred.upper"))

  # Test post-period that consists of only 1 time point
  cum.pred <- ComputeCumulativePredictions(y.samples, point.pred, y,
                                           post.period.begin = 100,
                                           alpha = 0.05)
  expect_equal(dim(cum.pred), c(100, 3))
  expect_equal(names(cum.pred), c("cum.pred", "cum.pred.lower",
                                  "cum.pred.upper"))

  # Test data `y` with missing data (NA) early in the pre-period.
  y.na <- y
  y.na[3] <- NA
  cum.pred <- ComputeCumulativePredictions(y.samples, point.pred, y.na,
                                           post.period.begin = 51,
                                           alpha = 0.05)
  expect_true(all(is.na(cum.pred[3, ])))
  expect_false(anyNA(cum.pred[-3, ]))

  # Test data with a missing values in the last time points before the
  # post-period.
  y.na <- y
  y.na[48 : 50] <- NA
  cum.pred <- ComputeCumulativePredictions(y.samples, point.pred, y.na,
                                           post.period.begin = 51,
                                           alpha = 0.05)
  expect_true(all(is.na(cum.pred[48 : 50, ])))
  expect_false(anyNA(cum.pred[-(48 : 50), ]))

  # Test that data with only missing values before the post-period throws an
  # error.
  y.na <- y
  y.na[1 : 50] <- NA
  expect_error(ComputeCumulativePredictions(y.samples, point.pred, y.na,
                                            post.period.begin = 51,
                                            alpha = 0.05),
               "length")
})

test_that("CompileSummaryTable", {
  CompileSummaryTable <- CausalImpact:::CompileSummaryTable

  # Test empty input
  expect_error(CompileSummaryTable())

  # Test some healthy input
  set.seed(1)
  y.post <- rnorm(100)
  y.samples.post <- matrix(rnorm(1000), nrow = 10)
  point.pred.mean.post <- rnorm(100)
  summary <- CompileSummaryTable(y.post, y.samples.post, point.pred.mean.post)
  expect_equal(names(summary), c("Actual", "Pred", "Pred.lower", "Pred.upper",
                                 "Pred.sd", "AbsEffect", "AbsEffect.lower",
                                 "AbsEffect.upper", "AbsEffect.sd",
                                 "RelEffect", "RelEffect.lower",
                                 "RelEffect.upper", "RelEffect.sd", "alpha",
                                 "p"))
  expect_equal(rownames(summary), c("Average", "Cumulative"))

  # Check some of the maths
  y.post <- rep(2, 10)
  y.samples.post <- matrix(1, nrow = 10, ncol = 10)
  point.pred.mean.post <- rep(1, 10)
  summary <- CompileSummaryTable(y.post, y.samples.post, point.pred.mean.post)
  expect_equal(summary$Actual, c(2, 20))
  expect_equal(summary$Pred, c(1, 10))
  expect_equal(summary$Pred.lower, c(1, 10))
  expect_equal(summary$Pred.upper, c(1, 10))
  expect_equal(summary$Pred.sd, c(0, 0))
  expect_equal(summary$AbsEffect, c(1, 10))
  expect_equal(summary$AbsEffect.lower, c(1, 10))
  expect_equal(summary$AbsEffect.upper, c(1, 10))
  expect_equal(summary$AbsEffect.sd, c(0, 0))
  expect_equal(summary$RelEffect, c(1, 1))
  expect_equal(summary$RelEffect.lower, c(1, 1))
  expect_equal(summary$RelEffect.upper, c(1, 1))
  expect_equal(summary$RelEffect.sd, c(0, 0))

  # Check inconsistent input
  expect_error(CompileSummaryTable(y.post[1 : 9], y.samples.post,
                                     point.pred.mean.post))
  expect_error(CompileSummaryTable(y.post, y.samples.post[, 1 : 9],
                                     point.pred.mean.post))
  expect_error(CompileSummaryTable(y.post, y.samples.post,
                                     point.pred.mean.post[1 : 9]))
})

test_that("InterpretSummaryTable", {
  InterpretSummaryTable <- CausalImpact:::InterpretSummaryTable
  CompileSummaryTable <- CausalImpact:::CompileSummaryTable

  # Test empty input
  expect_error(InterpretSummaryTable())

  # Test healthy input
  y.post <- rep(2, 10)
  y.samples.post <- matrix(1, nrow = 10, ncol = 10)
  point.pred.mean.post <- rep(1, 10)
  summary <- CompileSummaryTable(y.post, y.samples.post, point.pred.mean.post)
  stmt <- InterpretSummaryTable(summary)
  expect_true(nchar(stmt) > 500)
})

test_that("CheckInputForCompilePosteriorInferences", {
  CheckInputForCompilePosteriorInferences <-
      CausalImpact:::CheckInputForCompilePosteriorInferences
  ConstructModel <- CausalImpact:::ConstructModel

  # Test empty input
  expect_error(CheckInputForCompilePosteriorInferences(), "missing")

  # Define healthy input
  data <- zoo(cbind(c(rnorm(100), rep(NA, 100)),
                    rnorm(200), rnorm(200)))
  model.args <- list(niter = 100)
  suppressWarnings(bsts.model <- ConstructModel(data, model.args))
  y.cf <- rnorm(100)
  post.period <- c(151L, 180L)
  alpha <- 0.05
  UnStandardize <- identity

  # Test healthy input
  result <- CheckInputForCompilePosteriorInferences(bsts.model, y.cf,
                                                    post.period, alpha,
                                                    UnStandardize)
  expected <- list(
      bsts.model = bsts.model,
      y.cf = y.cf,
      post.period = post.period,
      alpha = alpha,
      UnStandardize = UnStandardize
  )
  expect_equal(result, expected)

  # Test bad <bsts.model>
  bad.bsts.model <- list(NULL, NA, rnorm(100))
  invisible(lapply(bad.bsts.model, function(bsts.model) {
    expect_error(CheckInputForCompilePosteriorInferences(bsts.model, y.cf,
                                                         post.period, alpha,
                                                         UnStandardize),
                 "bsts")
  }))

  # Test bad <y.cf>
  bad.y.cf <- list(NULL, "foo", data.frame(y.cf = y.cf), c(y.cf, 1, 2),
                   NA * y.cf, as.numeric(NA) * y.cf)
  invisible(lapply(bad.y.cf, function(y.cf) {
    expect_error(CheckInputForCompilePosteriorInferences(bsts.model, y.cf,
                                                         post.period, alpha,
                                                         UnStandardize))
  }))

  # Test bad <post.period>
  bad.post.period <- list(NULL, "foo", 1:3, c(30, 45), c(180, 150),
                          c(181, 220), as.integer(rep(NA, 2)))
  invisible(lapply(bad.post.period, function(post.period) {
    expect_error(CheckInputForCompilePosteriorInferences(bsts.model, y.cf,
                                                         post.period, alpha,
                                                         UnStandardize),
                 "post.period", fixed = TRUE)
  }))

  # Test bad <alpha>
  bad.alpha <- list(NA, as.numeric(NA), -1, 0, 1, c(0.8, 0.9))
  invisible(lapply(bad.alpha, function(alpha) {
    expect_error(CheckInputForCompilePosteriorInferences(bsts.model, y.cf,
                                                         post.period, alpha,
                                                         UnStandardize),
                 "alpha")
  }))

  # Test bad <UnStandardize>
  bad.UnStandardize <- list(NA, 1, "foo", toString, function(x) NULL,
                            function(x) c(x, x))
  invisible(lapply(bad.UnStandardize, function(UnStandardize) {
    expect_error(CheckInputForCompilePosteriorInferences(bsts.model, y.cf,
                                                         post.period, alpha,
                                                         UnStandardize),
                 "UnStandardize")
  }))
})

test_that("CompilePosteriorInferences", {
  CompilePosteriorInferences <- CausalImpact:::CompilePosteriorInferences
  ConstructModel <- CausalImpact:::ConstructModel

  # Test empty input
  expect_error(CompilePosteriorInferences(), "missing")

  # Test healthy input
  set.seed(1)
  data <- zoo(cbind(c(rnorm(100), rep(NA, 100)),
                    rnorm(200), rnorm(200)))
  model.args <- list(niter = 100)
  suppressWarnings(bsts.model <- ConstructModel(data, model.args))
  y.cf <- data[101 : 200, 1]
  coredata(y.cf) <- rnorm(100) + 100
  post.period <- c(151L, 180L)
  alpha <- 0.05
  UnStandardize <- identity
  inferences <- CompilePosteriorInferences(bsts.model, y.cf, post.period,
                                           alpha, UnStandardize)
  expect_equal(names(inferences), c("series", "summary", "report"))
  expect_equal(inferences$series$y.model, zoo(rbind(data[1 : 100, 1], y.cf)))
  expected.series.columns <-
    c("y.model", "cum.y.model",
      "point.pred", "point.pred.lower", "point.pred.upper",
      "cum.pred", "cum.pred.lower", "cum.pred.upper",
      "point.effect", "point.effect.lower", "point.effect.upper",
      "cum.effect", "cum.effect.lower", "cum.effect.upper")
  expect_equal(colnames(inferences$series), expected.series.columns)
  effect.cols <- grep("(point|cum)\\.effect", names(inferences$series))
  na.rows <- c(101L:150L, 181L:200L)
  expect_true(all(is.na(inferences$series[na.rows, effect.cols])))
  expect_false(anyNA(inferences$series[-na.rows, effect.cols]))
  expect_false(anyNA(inferences$series[, -effect.cols]))

  # Test different <alpha>
  alpha <- 0.1
  inferences2 <- CompilePosteriorInferences(bsts.model, y.cf, post.period,
                                            alpha, UnStandardize)
  # TODO(kbrodersen) Compare summary intervals.
})

test_that("CompileNaInferences", {
  CompileNaInferences <- CausalImpact:::CompileNaInferences

  # Test empty input
  expect_error(CompileNaInferences())

  # Test healty input
  result <- CompileNaInferences(zoo(c(1, 2, 3)))
  expect_equal(names(result), c("series", "summary", "report"))
  expect_true(is.zoo(result$series))
  expect_equal(nrow(result$series), 3)
  expect_equal(ncol(result$series), 14)
  expect_equal(as.vector(result$series$y.model), c(1, 2, 3))
  expect_equal(as.vector(result$series$cum.y.model), cumsum(c(1, 2, 3)))
  expect_equal(result$summary, NULL)
})
