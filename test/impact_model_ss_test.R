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


# Unit tests for impact_model_ss.R.
#
# Author: kbrodersen@google.com (Kay Brodersen)

# ------------------------------------------------------------------------------
TestObservationsAreIllConditioned <- function() {
  ObservationsAreIllConditioned <- CausalImpact:::ObservationsAreIllConditioned

  # Test without arguments
  checkException(ObservationsAreIllConditioned())

  # Test healthy input
  checkTrue(!ObservationsAreIllConditioned(c(1, 2, 3)))
  checkTrue(!ObservationsAreIllConditioned(c(1, 2, 3, NA, NA)))
  checkTrue(!ObservationsAreIllConditioned(c(NA, NA, 1, NA, 2, 3, NA, NA)))

  # Test all NA
  checkTrue(ObservationsAreIllConditioned(c(NA, NA, NA, NA, NA)))

  # Test fewer than 3 non-NA values
  checkException(ObservationsAreIllConditioned(NULL))
  checkException(ObservationsAreIllConditioned(c()))
  checkTrue(ObservationsAreIllConditioned(c(1)))
  checkTrue(ObservationsAreIllConditioned(c(1, 2)))
  checkTrue(ObservationsAreIllConditioned(c(1, 2, NA)))
  checkTrue(ObservationsAreIllConditioned(c(NA, 1, 2, NA)))
  checkTrue(ObservationsAreIllConditioned(c(NA, 1, 2, NA, NA)))
}

# ------------------------------------------------------------------------------
TestFormatInputForConstructModel <- function() {
  FormatInputForConstructModel <- CausalImpact:::FormatInputForConstructModel

  # Test without arguments
  checkException(FormatInputForConstructModel())

  # Specify some healthy input
  data <- zoo(cbind(rnorm(1000), rnorm(1000), rnorm(1000)))
  names(data) <- c("a", "b", "c")
  model.args <- list(niter = 1000,
                     standardize.data = TRUE,
                     prior.level.sd = 0.01,
                     nseasons = 1,
                     season.duration = 1,
                     dynamic.regression = FALSE)

  # Test normal input
  checkEquals(FormatInputForConstructModel(data, model.args),
              list(data = data, model.args = model.args))

  # Test that column names are assigned to <data> if it has no colum nmaes
  anon.data <- zoo(cbind(rnorm(1000), rnorm(1000), rnorm(1000)))
  expected.data <- anon.data
  names(expected.data) <- c("y", "x1", "x2")
  checkEquals(FormatInputForConstructModel(anon.data, model.args)$data,
              expected.data)

  # Test illegal extra args
  checkException(FormatInputForConstructModel(data, list(foo.extra.arg = 123)))

  # Test bad <data>
  bad.data <- list(NULL, zoo(cbind(c(1, 2, 3, 4, 5), c(6, 7, 8, NA, NA))), NA)
  lapply(bad.data, function(data) {
    checkException(FormatInputForConstructModel(data, model.args)) })

  # Test bad <model.args> as a whole
  bad.model.args <- list(list(niterFoo = 10), NA, as.numeric(NA), 1, c(1, 2, 3))
  lapply(bad.model.args, function(model.args) {
    checkException(FormatInputForConstructModel(data, model.args)) })

  # Test bad <niter>
  bad.niter <- list(NA, as.numeric(NA), -1, 9, 9.1, "foo", c(100, 200))
  lapply(bad.niter, function(niter) {
    checkException(FormatInputForConstructModel(data, list(niter = niter))) })

  # Test bad <prior.level.sd>
  bad.prior.level.sd <- list(NA, as.numeric(NA), -1, 0, "foo", c(100, 200))
  lapply(bad.prior.level.sd, function(prior.level.sd) {
    checkException(FormatInputForConstructModel(data, list(prior.level.sd =
                                                           prior.level.sd))) })

  # Test bad <nseasons>
  bad.nseasons <- list(0, NA, as.numeric(NA), -1, 9.1, "foo", c(100, 200))
  lapply(bad.nseasons, function(nseasons) {
    checkException(FormatInputForConstructModel(data,
                                                list(nseasons = nseasons)))
  })

  # Test bad <season.duration>
  bad.season.duration <- list(0, NA, as.numeric(NA), -1, 9.1, "foo",
                              c(100, 200))
  lapply(bad.season.duration, function(season.duration) {
    checkException(FormatInputForConstructModel(data, list(season.duration =
                                                           season.duration))) })

  # Test bad <dynamic.regression>
  bad.dynamic.regression <- list(NA, as.numeric(NA), 123, "foo", c(TRUE, FALSE))
  lapply(bad.dynamic.regression, function(dynamic.regression) {
    checkException(FormatInputForConstructModel(data,
                                                list(dynamic.regression =
                                                     dynamic.regression))) })
}

# ------------------------------------------------------------------------------
TestConstructModel <- function() {
  ConstructModel <- CausalImpact:::ConstructModel

  # Test without arguments
  checkException(ConstructModel())

  # Create some data in various formats; they should all lead to the same result
  set.seed(1)
  data0 <- zoo(cbind(rnorm(100), rnorm(100), rnorm(100)))
  data1 <- data0
  names(data1) <- c("a", "b", "c")
  data2 <- data0
  names(data2) <- c("y", "x1", "x2")
  some.data <- list(data0, data1, data2, as.data.frame(data0),
                    as.data.frame(data1), as.data.frame(data2),
                    coredata(data0))
  model.args <- list(niter = 100)

  # Test that ill-conditioned input leads to a NULL result
  lapply(some.data, function(data) {
    bsts.model <- ConstructModel(data * 0 + 1, model.args)
    checkTrue(is.null(bsts.model))
  })

  # Test no regression
  expected.model <- ConstructModel(data0[, 1], model.args)
  lapply(some.data, function(data) {
    bsts.model <- ConstructModel(data[, 1], model.args)
    checkTrue(!is.null(bsts.model))
    checkEquals(class(bsts.model), "bsts")
    checkTrue(all(bsts.model$original.series == data[, 1]))
    checkTrue(all(bsts.model$mf[, 1] == data[, 1]))
    checkEquals(bsts.model$state.contributions,
                expected.model$state.contributions)
  })

  # Test static regression
  expected.model <- ConstructModel(data0, model.args)
  lapply(some.data, function(data) {
    bsts.model <- ConstructModel(data, model.args)
    checkTrue(!is.null(bsts.model))
    checkEquals(class(bsts.model), "bsts")
    checkTrue(all(bsts.model$original.series == data[, 1]))
    checkTrue(all(bsts.model$mf[, 1] == data[, 1]))
    checkTrue(all(bsts.model$mf[, 2] == data[, 2]))
    checkTrue(all(bsts.model$mf[, 3] == data[, 3]))
    checkEquals(bsts.model$state.contributions,
                expected.model$state.contributions)
  })

  # Test dynamic regression
  expected.model <- ConstructModel(data0,
                                   list(niter = 100, dynamic.regression = TRUE))
  lapply(some.data, function(data) {
    bsts.model <- ConstructModel(data,
                                 list(niter = 100, dynamic.regression = TRUE))
    checkTrue(!is.null(bsts.model))
    checkEquals(class(bsts.model), "bsts")
    checkTrue(all(bsts.model$original.series == data[, 1]))
    checkTrue(all(bsts.model$mf[, 1] == data[, 1]))
    checkTrue(all(bsts.model$mf[, 2] == data[, 2]))
    checkTrue(all(bsts.model$mf[, 3] == data[, 3]))
    checkEquals(bsts.model$state.contributions,
                expected.model$state.contributions)
  })
}
