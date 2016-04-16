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

# Unit tests for impact_inference.R.
#
# Author: kbrodersen@google.com (Kay Brodersen)

TestGetPosteriorStateSamples <- function() {
  GetPosteriorStateSamples <- CausalImpact:::GetPosteriorStateSamples
  ConstructModel <- CausalImpact:::ConstructModel

  # Test empty input
  checkException(GetPosteriorStateSamples())

  # Generate some input
  data <- zoo(cbind(rnorm(365), rnorm(365), rnorm(365)))
  model.args <- list(niter = 100)

  # Create a healthy bsts.object and test it
  bsts.object <- ConstructModel(data, model.args)
  state.samples <- GetPosteriorStateSamples(bsts.object)
  checkEquals(ncol(state.samples), 365)
}

TestComputeResponseTrajectories <- function() {
  ComputeResponseTrajectories <- CausalImpact:::ComputeResponseTrajectories
  ConstructModel <- CausalImpact:::ConstructModel

  # Test empty input
  checkException(ComputeResponseTrajectories())

  # Test healthy input
  data <- zoo(cbind(rnorm(365), rnorm(365), rnorm(365)))
  model.args <- list(niter = 100)
  bsts.object <- ConstructModel(data, model.args)
  y.samples <- ComputeResponseTrajectories(bsts.object)
  checkEquals(ncol(y.samples), 365)
}

TestComputePointPredictions <- function() {
  ComputePointPredictions <- CausalImpact:::ComputePointPredictions

  # Test empty input
  checkException(ComputePointPredictions())

  # Test healthy input
  y.samples <- matrix(rnorm(1000), nrow = 10)
  state.samples <- matrix(rnorm(1000), nrow = 10)
  point.pred <- ComputePointPredictions(y.samples, state.samples, 0.05)
  checkEquals(dim(point.pred), c(100, 3))
  checkEquals(names(point.pred), c("point.pred", "point.pred.lower",
                                   "point.pred.upper"))
}

TestComputeCumulativePredictions <- function() {
  ComputeCumulativePredictions <- CausalImpact:::ComputeCumulativePredictions

  # Test empty input
  checkException(ComputeCumulativePredictions())

  # Test some healthy input
  y.samples <- matrix(rnorm(1000), nrow = 10)
  point.pred <- data.frame(point.pred = rnorm(100),
                           point.pred.lower = rnorm(100),
                           point.pred.upper = rnorm(100))
  y <- rnorm(100)
  cum.pred <- ComputeCumulativePredictions(y.samples, point.pred, y,
                                           post.period.begin = 51,
                                           alpha = 0.05)
  checkEquals(dim(cum.pred), c(100, 3))
  checkEquals(names(cum.pred), c("cum.pred", "cum.pred.lower",
                                 "cum.pred.upper"))

  # Test post-period that consists of only 1 time point
  cum.pred <- ComputeCumulativePredictions(y.samples, point.pred, y,
                                           post.period.begin = 100,
                                           alpha = 0.05)
  checkEquals(dim(cum.pred), c(100, 3))
  checkEquals(names(cum.pred), c("cum.pred", "cum.pred.lower",
                                 "cum.pred.upper"))

  # Test data <y> with missing data (NA) in pre-period
  y[3] <- NA
  cum.pred <- ComputeCumulativePredictions(y.samples, point.pred, y,
                                           post.period.begin = 51,
                                           alpha = 0.05)
  checkTrue(all(is.na(cum.pred[3, ])))
  checkTrue(all(!is.na(cum.pred[-3, ])))
}

TestCompileSummaryTable <- function() {
  CompileSummaryTable <- CausalImpact:::CompileSummaryTable

  # Test empty input
  checkException(CompileSummaryTable())

  # Test some healthy input
  y.post <- rnorm(100)
  y.samples.post <- matrix(rnorm(1000), nrow = 10)
  point.pred.mean.post <- rnorm(100)
  summary <- CompileSummaryTable(y.post, y.samples.post, point.pred.mean.post)
  checkEquals(names(summary), c("Actual", "Pred", "Pred.lower", "Pred.upper",
                                "Pred.sd", "AbsEffect", "AbsEffect.lower",
                                "AbsEffect.upper", "AbsEffect.sd",
                                "RelEffect", "RelEffect.lower",
                                "RelEffect.upper", "RelEffect.sd", "alpha",
                                "p"))
  checkEquals(rownames(summary), c("Average", "Cumulative"))

  # Check some of the maths
  y.post <- rep(2, 10)
  y.samples.post <- matrix(1, nrow = 10, ncol = 10)
  point.pred.mean.post <- rep(1, 10)
  summary <- CompileSummaryTable(y.post, y.samples.post, point.pred.mean.post)
  checkEquals(summary$Actual, c(2, 20))
  checkEquals(summary$Pred, c(1, 10))
  checkEquals(summary$Pred.lower, c(1, 10))
  checkEquals(summary$Pred.upper, c(1, 10))
  checkEquals(summary$Pred.sd, c(0, 0))
  checkEquals(summary$AbsEffect, c(1, 10))
  checkEquals(summary$AbsEffect.lower, c(1, 10))
  checkEquals(summary$AbsEffect.upper, c(1, 10))
  checkEquals(summary$AbsEffect.sd, c(0, 0))
  checkEquals(summary$RelEffect, c(1, 1))
  checkEquals(summary$RelEffect.lower, c(1, 1))
  checkEquals(summary$RelEffect.upper, c(1, 1))
  checkEquals(summary$RelEffect.sd, c(0, 0))

  # Check inconsistent input
  checkException(CompileSummaryTable(y.post[1 : 9], y.samples.post,
                                     point.pred.mean.post))
  checkException(CompileSummaryTable(y.post, y.samples.post[, 1 : 9],
                                     point.pred.mean.post))
  checkException(CompileSummaryTable(y.post, y.samples.post,
                                     point.pred.mean.post[1 : 9]))
}

TestInterpretSummaryTable <- function() {
  InterpretSummaryTable <- CausalImpact:::InterpretSummaryTable
  CompileSummaryTable <- CausalImpact:::CompileSummaryTable

  # Test empty input
  checkException(InterpretSummaryTable())

  # Test healthy input
  y.post <- rep(2, 10)
  y.samples.post <- matrix(1, nrow = 10, ncol = 10)
  point.pred.mean.post <- rep(1, 10)
  summary <- CompileSummaryTable(y.post, y.samples.post, point.pred.mean.post)
  stmt <- InterpretSummaryTable(summary)
  checkTrue(nchar(stmt) > 500)
}

TestCheckInputForCompilePosteriorInferences <- function() {
  CheckInputForCompilePosteriorInferences <-
      CausalImpact:::CheckInputForCompilePosteriorInferences
  ConstructModel <- CausalImpact:::ConstructModel

  # Test empty input
  checkException(CheckInputForCompilePosteriorInferences())

  # Define healthy input
  data <- zoo(cbind(c(rnorm(100), rep(NA, 100)),
                    rnorm(200), rnorm(200)))
  model.args <- list(niter = 100)
  bsts.object <- ConstructModel(data, model.args)
  y.post <- rnorm(100)
  alpha <- 0.05
  UnStandardize <- identity

  # Test bad <bsts.object>
  bad.bsts.object <- list(NULL, NA, rnorm(100))
  lapply(bad.bsts.object, function(bsts.object) {
    checkException(CheckInputForCompilePosteriorInferences(bsts.object, y.post,
                                                           alpha,
                                                           UnStandardize)) })

  # Test bad <y.post>
  bad.y.post <- list(NULL, "foo", data.frame(y.post = y.post), c(y.post, 1, 2),
                     NA * y.post, as.numeric(NA) * y.post)
  lapply(bad.y.post, function(y.post) {
    checkException(CheckInputForCompilePosteriorInferences(bsts.object, y.post,
                                                           alpha,
                                                           UnStandardize)) })

  # Test bad <alpha>
  bad.alpha <- list(NA, as.numeric(NA), -1, 0, 1, c(0.8, 0.9))
  lapply(bad.alpha, function(alpha) {
    checkException(CheckInputForCompilePosteriorInferences(bsts.object, y.post,
                                                           alpha,
                                                           UnStandardize)) })

  # Test bad <UnStandardize>
  bad.UnStandardize <- list(NA, 1, "foo", toString, function(x) NULL,
                            function(x) c(x, x))
  lapply(bad.UnStandardize, function(UnStandardize) {
    checkException(CheckInputForCompilePosteriorInferences(bsts.object, y.post,
                                                           alpha,
                                                           UnStandardize)) })
}

TestCompilePosteriorInferences <- function() {
  CompilePosteriorInferences <- CausalImpact:::CompilePosteriorInferences
  ConstructModel <- CausalImpact:::ConstructModel

  # Test empty input
  checkException(CompilePosteriorInferences())

  # Test healthy input
  data <- zoo(cbind(c(rnorm(100), rep(NA, 100)),
                    rnorm(200), rnorm(200)))
  model.args <- list(niter = 100)
  bsts.object <- ConstructModel(data, model.args)
  y.post <- data[101 : 200, 1]
  coredata(y.post) <- rnorm(100) + 100
  alpha <- 0.05
  UnStandardize <- identity
  inferences <- CompilePosteriorInferences(bsts.object, y.post, alpha,
                                           UnStandardize)
  checkEquals(names(inferences), c("series", "summary", "report"))
  checkEquals(inferences$series$y, zoo(rbind(data[1 : 100, 1], y.post)))
  checkTrue(!anyNA(inferences$series))

  # Test different <alpha>
  alpha <- 0.1
  inferences2 <- CompilePosteriorInferences(bsts.object, y.post, alpha,
                                            UnStandardize)
  # TODO(kbrodersen) Compare summary intervals.
}

TestCompileNaInferences <- function() {
  CompileNaInferences <- CausalImpact:::CompileNaInferences

  # Test empty input
  checkException(CompileNaInferences())

  # Test healty input
  result <- CompileNaInferences(zoo(c(1, 2, 3)))
  checkEquals(names(result), c("series", "summary", "report"))
  checkTrue(is.zoo(result$series))
  checkEquals(nrow(result$series), 3)
  checkEquals(ncol(result$series), 14)
  checkEquals(as.vector(result$series$y.model), c(1, 2, 3))
  checkEquals(as.vector(result$series$cum.y.model), cumsum(c(1, 2, 3)))
  checkEquals(result$summary, NULL)
}
