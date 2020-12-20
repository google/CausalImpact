# Copyright 2014-2020 Google Inc. All rights reserved.
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

testthat::context("Unit tests for impact_model_ss.R")

# Author: kbrodersen@google.com (Kay Brodersen)

test_that("ObservationsAreIllConditioned", {
  ObservationsAreIllConditioned <- CausalImpact:::ObservationsAreIllConditioned

  # Test without arguments
  expect_error(ObservationsAreIllConditioned())

  # Test healthy input
  expect_false(ObservationsAreIllConditioned(c(1, 2, 3)))
  expect_false(ObservationsAreIllConditioned(c(1, 2, 3, NA, NA)))
  expect_false(ObservationsAreIllConditioned(c(NA, NA, 1, NA, 2, 3, NA, NA)))

  # Test all NA
  expect_warning(ObservationsAreIllConditioned(c(NA, NA, NA, NA, NA)), "all NA")
  expect_true(suppressWarnings(
    ObservationsAreIllConditioned(c(NA, NA, NA, NA, NA))))

  # Test fewer than 3 non-NA values
  expect_error(ObservationsAreIllConditioned(NULL))
  expect_error(ObservationsAreIllConditioned(c()))
  expect_warning(ObservationsAreIllConditioned(c(1)),
                 "fewer than 3 non-NA values")
  expect_true(suppressWarnings(ObservationsAreIllConditioned(c(1))))
  expect_warning(ObservationsAreIllConditioned(c(1, 2)),
                 "fewer than 3 non-NA values")
  expect_true(suppressWarnings(ObservationsAreIllConditioned(c(1, 2))))
  expect_warning(ObservationsAreIllConditioned(c(1, 2, NA)),
                 "fewer than 3 non-NA values")
  expect_true(suppressWarnings(ObservationsAreIllConditioned(c(1, 2, NA))))
  expect_warning(ObservationsAreIllConditioned(c(NA, 1, 2, NA)),
                 "fewer than 3 non-NA values")
  expect_true(suppressWarnings(ObservationsAreIllConditioned(c(NA, 1, 2, NA))))
  expect_warning(ObservationsAreIllConditioned(c(NA, 1, 2, NA, NA)),
                 "fewer than 3 non-NA values")
  expect_true(suppressWarnings(
    ObservationsAreIllConditioned(c(NA, 1, 2, NA, NA))))
})

test_that("FormatInputForConstructModel", {
  FormatInputForConstructModel <- CausalImpact:::FormatInputForConstructModel

  # Test without arguments
  expect_error(FormatInputForConstructModel())

  # Specify some healthy input
  data <- zoo(cbind(rnorm(1000), rnorm(1000), rnorm(1000)))
  names(data) <- c("a", "b", "c")
  model.args <- list(niter = 1000,
                     standardize.data = TRUE,
                     prior.level.sd = 0.01,
                     nseasons = 1,
                     season.duration = 1,
                     dynamic.regression = FALSE,
                     max.flips = 100)

  # Test normal input
  expect_equal(FormatInputForConstructModel(data, model.args),
              list(data = data, model.args = model.args))

  # Test that column names are assigned to <data> if it has no column names
  anon.data <- zoo(cbind(rnorm(1000), rnorm(1000), rnorm(1000)))
  expected.data <- anon.data
  names(expected.data) <- c("y", "x1", "x2")
  expect_equal(FormatInputForConstructModel(anon.data, model.args)$data,
              expected.data)

  # Test illegal extra args
  expect_error(FormatInputForConstructModel(data, list(foo.extra.arg = 123)))

  # Test bad <data>
  bad.data <- list(NULL, zoo(cbind(c(1, 2, 3, 4, 5), c(6, 7, 8, NA, NA))), NA)
  lapply(bad.data, function(data) {
    expect_error(FormatInputForConstructModel(data, model.args)) })

  # Test bad <model.args> as a whole
  bad.model.args <- list(list(niterFoo = 10), NA, as.numeric(NA), 1, c(1, 2, 3))
  lapply(bad.model.args, function(model.args) {
    expect_error(FormatInputForConstructModel(data, model.args)) })

  # Test bad <niter>
  bad.niter <- list(NA, as.numeric(NA), -1, 9, 9.1, "foo", c(100, 200))
  lapply(bad.niter, function(niter) {
    expect_error(FormatInputForConstructModel(data, list(niter = niter))) })

  # Test bad <prior.level.sd>
  bad.prior.level.sd <- list(NA, as.numeric(NA), -1, 0, "foo", c(100, 200))
  lapply(bad.prior.level.sd, function(prior.level.sd) {
    expect_error(FormatInputForConstructModel(data, list(prior.level.sd =
                                                           prior.level.sd))) })

  # Test bad <nseasons>
  bad.nseasons <- list(0, NA, as.numeric(NA), -1, 9.1, "foo", c(100, 200))
  lapply(bad.nseasons, function(nseasons) {
    expect_error(FormatInputForConstructModel(data,
                                              list(nseasons = nseasons)))
  })

  # Test bad <season.duration>
  bad.season.duration <- list(0, NA, as.numeric(NA), -1, 9.1, "foo",
                              c(100, 200))
  lapply(bad.season.duration, function(season.duration) {
    expect_error(FormatInputForConstructModel(data, list(season.duration =
                                                           season.duration))) })

  # Test bad <dynamic.regression>
  bad.dynamic.regression <- list(NA, as.numeric(NA), 123, "foo", c(TRUE, FALSE))
  lapply(bad.dynamic.regression, function(dynamic.regression) {
    expect_error(FormatInputForConstructModel(data,
                                              list(dynamic.regression =
                                                dynamic.regression))) })

  # Test bad <max.flips>
  bad.max.flips <- list(-2, 9.1, "foo", c(100, 200))
  lapply(bad.max.flips, function(max.flips) {
    print(max.flips)
    expect_error(FormatInputForConstructModel(data,
                                              list(max.flips = max.flips))) })
})

test_that("ConstructModel", {
  ConstructModel <- CausalImpact:::ConstructModel

  # Test without arguments
  expect_error(ConstructModel())

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
    suppressWarnings(bsts.model <- ConstructModel(data * 0 + 1, model.args))
    expect_true(is.null(bsts.model))
  })

  # Test no regression
  suppressWarnings(expected.model <- ConstructModel(data0[, 1], model.args))
  lapply(some.data, function(data) {
    suppressWarnings(bsts.model <- ConstructModel(data[, 1], model.args))
    expect_false(is.null(bsts.model))
    expect_equal(class(bsts.model), "bsts")
    expect_equal(as.numeric(bsts.model$original.series), as.numeric(data[, 1]))
    expect_equal(bsts.model$state.contributions,
                 expected.model$state.contributions)
  })

  # Test static regression
  suppressWarnings(expected.model <- ConstructModel(data0, model.args))
  lapply(some.data, function(data) {
    suppressWarnings(bsts.model <- ConstructModel(data, model.args))
    expect_false(is.null(bsts.model))
    expect_equal(class(bsts.model), "bsts")
    expect_equal(as.numeric(bsts.model$original.series), as.numeric(data[, 1]))
    expect_equivalent(bsts.model$predictors[, 1], rep(1, nrow(data)))
    expect_equivalent(bsts.model$predictors[, 2], as.numeric(data[, 2]))
    expect_equivalent(bsts.model$predictors[, 3], as.numeric(data[, 3]))
    expect_equal(bsts.model$state.contributions,
                 expected.model$state.contributions)
  })

  # Test dynamic regression
  suppressWarnings(expected.model <-
      ConstructModel(data0, list(niter = 100, dynamic.regression = TRUE)))
  lapply(some.data, function(data) {
    suppressWarnings(bsts.model <-
        ConstructModel(data, list(niter = 100, dynamic.regression = TRUE)))
    expect_false(is.null(bsts.model))
    expect_equal(class(bsts.model), "bsts")
    expect_equal(as.numeric(bsts.model$original.series), as.numeric(data[, 1]))
    expect_equal(
        as.numeric(bsts.model$state.specification[[2]]$predictors[, 1]),
        as.numeric(data[, 2]))
    expect_equal(
        as.numeric(bsts.model$state.specification[[2]]$predictors[, 2]),
        as.numeric(data[, 3]))
    expect_equal(bsts.model$state.contributions,
                 expected.model$state.contributions)
  })
})
