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

testthat::context("Unit tests for impact_analysis.R")

# Authors: kbrodersen@google.com (Kay H. Brodersen)
#          gallusser@google.com (Fabian Gallusser)

.expected.series.columns <-
    c("response", "cum.response",
      "point.pred", "point.pred.lower", "point.pred.upper",
      "cum.pred", "cum.pred.lower", "cum.pred.upper",
      "point.effect", "point.effect.lower", "point.effect.upper",
      "cum.effect", "cum.effect.lower", "cum.effect.upper")

CallAllS3Methods <- function(impact) {
  # This function tests the various object methods that are implemented in
  # impact_analysis.R for objects of class 'CausalImpact'.

  # If summary(impact, "foo")) does not generate an exception as expected, this
  # is probably because summary.default() is called rather than
  # summary.CausalImpact(). Make sure <impact> is of class <CausalImpact>, and
  # make sure summary.CausalImpact() is declared as an S3method export in the
  # NAMESPACE file.

  expect_output(summary(impact), "Posterior inference")
  expect_output(summary(impact, output = "summary"), "Posterior inference")
  expect_output(summary(impact, output = "report"), "Analysis report")
  expect_output(summary(impact, "summary"), "Posterior inference")
  expect_output(summary(impact, "report"), "Analysis report")
  expect_output(summary(impact, "s"), "Posterior inference")
  expect_error(summary(impact, "foo"), "summary")
  expect_output(print(impact), "Posterior inference")
  expect_output(print(impact, "summary"), "Posterior inference")
  expect_output(print(impact, "report"), "Analysis report")
  expect_output(print(impact, "s"), "Posterior inference")
  expect_error(print(impact, "foo"), "summary")
  expect_error(plot(impact), NA)
  expect_error(q <- plot(impact), NA)
  expect_error(plot(q), NA)
}

test_that("FormatInputForCausalImpact", {
  FormatInputForCausalImpact <- CausalImpact:::FormatInputForCausalImpact

  # Test missing input
  expect_error(FormatInputForCausalImpact(), "missing")

  # Specify some healthy input variables
  data <- zoo(data.frame(y = rnorm(200), x1 = rnorm(200), x2 = rnorm(200)))
  pre.period <- c(1, 100)
  post.period <- c(101, 200)
  model.args <- list(niter = 123)
  bsts.model <- list()
  class(bsts.model) <- "bsts"
  post.period.response <- rnorm(100)
  alpha <- 0.05

  # Test data input (usage scenario 1)
  expected <- list(data = data, pre.period = pre.period,
                   post.period = post.period, model.args = model.args,
                   bsts.model = NULL, post.period.response = NULL,
                   alpha = alpha)
  result <- FormatInputForCausalImpact(data, pre.period, post.period,
                                       model.args, NULL, NULL, alpha)
  expect_equal(result[-4], expected[-4])
  expect_equal(result$model.args$niter, model.args$niter)

  # Test bsts.model input (usage scenario 2)
  expected <- list(data = NULL, pre.period = NULL,
                   post.period = NULL, model.args = NULL,
                   bsts.model = bsts.model,
                   post.period.response = post.period.response, alpha = alpha)
  checked <- FormatInputForCausalImpact(NULL, NULL, NULL, NULL,
                                        bsts.model, post.period.response, alpha)
  expect_equal(checked[-4], expected[-4])

  # Test inconsistent input (must not provide both data and bsts.model)
  expect_error(FormatInputForCausalImpact(data, pre.period, post.period,
                                          model.args, bsts.model,
                                          post.period.response, alpha))

  # Test that <data> is converted to zoo
  expected.data <- zoo(c(10, 20, 30, 40), c(1, 2, 3, 4))
  dim(expected.data) <- c(4, 1)
  funny.data <- list(zoo(c(10, 20, 30, 40)),
                     zoo(c(10, 20, 30, 40), c(1, 2, 3, 4)),
                     c(10, 20, 30, 40),
                     c(10L, 20L, 30L, 40L),
                     matrix(c(10, 20, 30, 40)))
  invisible(lapply(funny.data, function(data) {
    checked <- FormatInputForCausalImpact(data, c(1, 3), c(4, 4),
                                          model.args, NULL, NULL, alpha)
    expect_equal(checked$data, expected.data)
  }))

  # Test data frame input
  df.data <- data.frame(y = c(10, 20, 30, 40))
  expected.data <- as.zoo(df.data)
  checked <- FormatInputForCausalImpact(df.data, c(1, 3), c(4, 4),
                                        model.args, NULL, NULL, alpha)
  expect_equal(checked$data, expected.data)

  # Test bad <data>
  bad.data <- list(NULL, NA, as.numeric(NA), letters[1 : 10], list(1, 2, 3, 4),
                   matrix(letters[1 : 10], ncol = 2),
                   data.frame(y = letters[1 : 10], x = letters[11 : 20]))
  invisible(lapply(bad.data, function(data) {
    expect_error(FormatInputForCausalImpact(data, c(1, 3), c(4, 4),
                                            model.args, NULL, NULL, alpha))
  }))

  # Test bad <pre.period>
  bad.pre.period <- list(NULL, 1, c(1, 2, 3), c(NA, 2), NA,
                         as.Date(c("2011-01-01", "2011-12-31")))
  invisible(lapply(bad.pre.period, function(pre.period) {
    expect_error(FormatInputForCausalImpact(data, pre.period, post.period,
                                            model.args, NULL, NULL, alpha))
  }))

  # Test bad <post.period>
  bad.post.period <- list(NULL, 1, c(1, 2, 3), c(NA, 2), NA,
                          as.Date(c("2011-01-01", "2011-12-31")))
  invisible(lapply(bad.post.period, function(post.period) {
    expect_error(FormatInputForCausalImpact(data, pre.period, post.period,
                                            model.args, NULL, NULL, alpha))
  }))

  # Test bad combination of <pre.period> and <post.period>
  expect_error(FormatInputForCausalImpact(data, c(1, 5), c(3, 10), model.args,
                                          NULL, NULL, alpha))

  # Test what happens when pre.period/post.period has a different class than
  # the timestamps in <data>
  bad.data <- zoo(c(1, 2, 3, 4), as.Date(c("2014-01-01", "2014-01-02",
                                           "2014-01-03", "2014-01-04")))
  bad.pre.period <- c(1, 3)  # numeric
  bad.post.period <- c(4, 4)
  expect_error(FormatInputForCausalImpact(bad.data,
                                          bad.pre.period, bad.post.period,
                                          model.args, NULL, NULL, alpha))
  bad.pre.period <- c(1L, 3L)  # integer
  bad.post.period <- c(4L, 4L)
  expect_error(FormatInputForCausalImpact(bad.data,
                                          bad.pre.period, bad.post.period,
                                          model.args, NULL, NULL, alpha))
  ok.data <- zoo(c(1, 2, 3, 4))
  ok.pre.period <- c(1L, 3L)
  ok.post.period <- c(4L, 4L)
  expect_error(FormatInputForCausalImpact(ok.data, ok.pre.period,
                                          ok.post.period, model.args, NULL,
                                          NULL, alpha),
               NA)
  ok.data <- zoo(c(1, 2, 3, 4), c(1, 2, 3, 4))
  ok.pre.period <- c(1, 3)
  ok.post.period <- c(4, 4)
  expect_error(FormatInputForCausalImpact(ok.data, ok.pre.period,
                                          ok.post.period, model.args, NULL,
                                          NULL, alpha),
               NA)
  ok.data <- zoo(c(1, 2, 3, 4), c(1, 2, 3, 4))
  ok.pre.period <- c(1L, 3L)  # we'll convert integer to numeric
  ok.post.period <- c(4L, 4L)
  expect_error(FormatInputForCausalImpact(ok.data, ok.pre.period,
                                          ok.post.period, model.args, NULL,
                                          NULL, alpha),
               NA)
  ok.data <- zoo(c(1, 2, 3, 4))
  ok.pre.period <- c(1, 3)  # we'll convert numeric to integer
  ok.post.period <- c(4, 4)
  expect_error(FormatInputForCausalImpact(ok.data, ok.pre.period,
                                          ok.post.period, model.args, NULL,
                                          NULL, alpha),
               NA)

  # Test bad <model.args>
  bad.model.args <- list(1000, "niter = 1000")
  invisible(lapply(bad.model.args, function(model.args) {
    expect_error(FormatInputForCausalImpact(data, pre.period, post.period,
                                            model.args, NULL, NULL, alpha))
  }))

  # Test bad <standardize.data>
  bad.standardize.data <- list(NA, as.numeric(NA), 123, "foo", c(TRUE, FALSE))
  invisible(lapply(bad.standardize.data, function(standardize.data) {
    expect_error(FormatInputForCausalImpact(data, pre.period, post.period,
                                            list(standardize.data =
                                                 standardize.data),
                                            NULL, NULL, alpha))
  }))

  # Test bad <bsts.model>
  bad.bsts.model <- list(NULL, NA, 1, c(1, 2, 3))
  invisible(lapply(bad.bsts.model, function(bsts.model) {
    expect_error(FormatInputForCausalImpact(NULL, NULL, NULL, NULL, bsts.model,
                                            post.period.response, alpha))
  }))

  # Test bad <post.period.response>
  # (Note that consistency with bsts.model is not tested in
  # FormatInputForCausalImpact().)
  bad.post.period.response <- list(NULL, as.Date("2011-01-01"),
                                   data.frame(x = c(1, 2, 3)), TRUE)
  invisible(lapply(bad.post.period.response, function(post.period.response) {
    expect_error(FormatInputForCausalImpact(NULL, NULL, NULL, NULL, bsts.model,
                                            post.period.response, alpha))
  }))

  # Test bad <alpha>
  bad.alpha <- list(NULL, NA, as.numeric(NA), -1, 0, 1, c(0.8, 0.9), "0.1")
  invisible(lapply(bad.alpha, function(alpha) {
    expect_error(FormatInputForCausalImpact(data, pre.period, post.period,
                                            model.args, NULL, NULL, alpha))
  }))
})

test_that("CausalImpact.RunWithData.DataFormats", {
  # Test CausalImpact() on different input data formats.

  # Test missing input
  expect_error(CausalImpact(), "provide data")

  # Test anonymous zoo series
  set.seed(1)
  data <- zoo(cbind(rnorm(200), rnorm(200), rnorm(200)))
  pre.period <- c(1, 100)
  post.period <- c(101, 200)
  model.args <- list(niter = 100)
  suppressWarnings(impact <- CausalImpact(data, pre.period, post.period,
                                          model.args))
  expect_equal(names(impact), c("series", "summary", "report", "model"))
  expect_equal(names(impact$series), .expected.series.columns)
  expect_equal(nrow(impact$series), nrow(data))
  expect_equal(time(impact$series), time(data))
  CallAllS3Methods(impact)

  # Test other data formats
  set.seed(1)
  pre.period <- c(1, 100)
  post.period <- c(101, 200)
  model.args <- list(niter = 100)
  #
  # Vector
  data <- rnorm(200)
  suppressWarnings(impact <- CausalImpact(data, pre.period, post.period,
                                          model.args))
  expect_equal(names(impact$series)[1], "response")
  #
  # Named zoo series
  data <- zoo(cbind(rnorm(200), rnorm(200), rnorm(200)))
  names(data) <- c("a", "b", "c")
  suppressWarnings(impact <- CausalImpact(data, pre.period, post.period,
                                          model.args))
  expect_equal(names(impact$series)[1], "response")
  #
  # Zoo series with only 1 variable
  data <- rnorm(200)
  suppressWarnings(impact <- CausalImpact(data, pre.period, post.period,
                                          model.args))
  expect_equal(names(impact$series)[1], "response")
  #
  # Data frame
  data <- data.frame(a = rnorm(200), b = rnorm(200), c = rnorm(200))
  suppressWarnings(impact <- CausalImpact(data, pre.period, post.period,
                                          model.args))
  expect_equal(names(impact$series)[1], "response")
  #
  # Data frame with only 1 variable
  data <- data.frame(a = rnorm(200))
  suppressWarnings(impact <- CausalImpact(data, pre.period, post.period,
                                          model.args))
  expect_equal(names(impact$series)[1], "response")
  #
  # Matrix
  data <- matrix(rnorm(600), ncol = 3)
  suppressWarnings(impact <- CausalImpact(data, pre.period, post.period,
                                          model.args))
  expect_equal(names(impact$series)[1], "response")
  #
  # Matrix with only 1 column
  data <- matrix(rnorm(600), ncol = 1)
  suppressWarnings(impact <- CausalImpact(data, pre.period, post.period,
                                          model.args))
  expect_equal(names(impact$series)[1], "response")
})

test_that("CausalImpact.RunWithData.PreAndPostPeriod", {
  # Test different (non-standard) pre- and post-periods constellations.

  # Test missing data (NA) in pre-period response variable
  set.seed(1)
  data <- zoo(cbind(rnorm(200), rnorm(200), rnorm(200)))
  data[3:5, 1] <- NA
  pre.period <- c(1, 100)
  post.period <- c(101, 200)
  model.args <- list(niter = 100)
  suppressWarnings(impact <- CausalImpact(data, pre.period, post.period,
                                          model.args))
  expect_equal(nrow(impact$series), nrow(data))
  expect_equal(time(impact$series), time(data))
  unaffected.cols <- c("point.pred", "point.pred.lower", "point.pred.upper")
  for (na.col in unaffected.cols) {
    expect_false(anyNA(as.data.frame(impact$series)[[na.col]]))
  }
  for (na.col in setdiff(names(impact$series), unaffected.cols)) {
    expect_true(all(is.na(as.data.frame(impact$series)[[na.col]][3:5])))
    expect_false(anyNA(as.data.frame(impact$series)[[na.col]][-c(3:5)]))
  }
  CallAllS3Methods(impact)

  # Test pre-period that starts after the beginning of the time series.
  set.seed(1)
  data <- rnorm(20, mean = 100, sd = 10)
  pre.period <- c(3, 10)
  post.period <- c(11, 20)
  model.args <- list(niter = 100)
  suppressWarnings(impact <- CausalImpact(data, pre.period, post.period,
                                          model.args))
  expect_true(all(is.na(impact$series[1 : (pre.period[1] - 1), -c(1, 2)])))
  expect_equal(impact$series$response, zoo(data))

  # Test post-period that does not last until the end of the data. Note that
  # predictions are calculated beyond the post-period while effects are not.
  set.seed(1)
  data <- rnorm(20, mean = 100, sd = 10)
  pre.period <- c(1, 10)
  post.period <- c(11, 18)
  model.args <- list(niter = 100)
  suppressWarnings(impact <- CausalImpact(data, pre.period, post.period,
                                          model.args))
  expect_equal(impact$series$response, zoo(data))
  effect.cols <- grep("(point|cum)\\.effect", names(impact$series))
  expect_true(all(is.na(impact$series[c(19L, 20L), effect.cols])))
  expect_false(anyNA(impact$series[-c(19L, 20L), effect.cols]))
  expect_false(anyNA(impact$series[, -effect.cols]))

  # Test gap between pre.period and post.period
  set.seed(1)
  data <- rnorm(30, mean = 100, sd = 10)
  pre.period <- c(1, 10)
  post.period <- c(21, 30)
  gap <- 11:20
  model.args <- list(niter = 100)
  suppressWarnings(impact <- CausalImpact(data, pre.period, post.period,
                                          model.args))
  expect_true(all(is.na(impact$model$bsts.model$original.series[gap])))
  effect.cols <- grep("(point|cum)\\.effect", names(impact$series))
  expect_true(all(is.na(impact$series[gap, effect.cols])))
  expect_false(anyNA(impact$series[-gap, effect.cols]))
  expect_false(anyNA(impact$series[, -effect.cols]))

  # Test case combining previous test cases, having a pre-period that starts
  # after the beginning of the time series, a gap between pre- and post-period,
  # and a post-period that does not last until the end of the data.
  set.seed(1)
  data <- rnorm(50, mean = 100, sd = 10)
  pre.period <- c(11, 20)
  post.period <- c(31, 40)
  model.args <- list(niter = 100)
  suppressWarnings(impact <- CausalImpact(data, pre.period, post.period,
                                          model.args))
  expect_equal(time(impact$model$bsts.model$original.series), time(data)[11:50])
  # Test that only pre-period is used for model fitting
  expect_true(all(is.na(impact$model$bsts.model$original.series[11:40])))
  expect_equal(impact$series$response, zoo(data))
  # Test that effects are only calculated during pre- and post-period
  effect.cols <- grep("(point|cum)\\.effect", names(impact$series))
  expect_true(all(is.na(impact$series[c(21:30, 41:50), effect.cols])))
  expect_false(anyNA(impact$series[c(21:30, 41:50), -effect.cols]))

  # Same test case as before, this time using Date time points.
  set.seed(1)
  times <- seq.Date(as.Date("2014-01-01"), as.Date("2014-01-01") + 49, by = 1)
  data <- zoo(rnorm(50, mean = 100, sd = 10), times)
  pre.period <- times[c(11, 20)]
  post.period <- times[c(31, 40)]
  model.args <- list(niter = 100)
  suppressWarnings(impact <- CausalImpact(data, pre.period, post.period,
                                          model.args))
  expect_equal(time(impact$model$bsts.model$original.series), 11:50)
  # Test that only pre-period is used for model fitting
  expect_true(all(is.na(impact$model$bsts.model$original.series[11:40])))
  expect_equal(impact$series$response, zoo(data))
  # Test that effects are only calculated during pre- and post-period
  effect.cols <- grep("(point|cum)\\.effect", names(impact$series))
  expect_true(all(is.na(impact$series[c(21:30, 41:50), effect.cols])))
  expect_false(anyNA(impact$series[c(21:30, 41:50), -effect.cols]))
})

test_that("CausalImpact.RunWithData.MissingValues", {
  # Create a time series without gap between pre- and post-period. Test that
  # missing values in the last entries of the pre-period do not lead to
  # estimation errors.
  set.seed(42)
  data <- rnorm(30, mean = 100, sd = 10)
  data[18 : 20] <- NA
  pre.period <- c(1, 20)
  post.period <- c(21, 30)
  model.args <- list(niter = 100)
  expect_error(impact <- CausalImpact(data, pre.period, post.period,
                                      model.args),
               NA)
  # Test that all columns in the result series except those associated with
  # point predictions have missing values at the time points the result time
  # series has missing values.
  point.pred.cols <- grep("^point\\.pred", names(impact$series))
  expect_true(all(is.na(impact$series[18 : 20, -point.pred.cols])))
  expect_false(anyNA(impact$series[18 : 20, point.pred.cols]))
  expect_false(anyNA(impact$series[-(18 : 20), ]))

  # Create a time series with a gap between pre- and post-period, and fit three
  # CausalImpact models: one on the raw data, one after setting the last entries
  # of the gap to NA, and one after setting earlier entries of the gap to NA.
  # Test that the estimated effects of the 3 fits are nearly identical.
  set.seed(42)
  data <- rnorm(30, mean = 100, sd = 10)
  pre.period <- c(1, 10)
  post.period <- c(21, 30)
  model.args <- list(niter = 100)
  set.seed(1)
  suppressWarnings(impact <- CausalImpact(data, pre.period, post.period,
                                          model.args))
  data.na.end <- replace(data, 18 : 20, NA)
  set.seed(1)
  suppressWarnings(impact.na.end <- CausalImpact(data.na.end, pre.period,
                                                 post.period, model.args))
  data.na.middle <- replace(data, 12 : 15, NA)
  set.seed(1)
  suppressWarnings(impact.na.middle <- CausalImpact(data.na.middle, pre.period,
                                                    post.period, model.args))
  # Do not compare columns with cumulative response (which is impacted by NAs in
  # response) and cumulative predictions (which are set to cumulative response
  # before the post-period.)
  exclude.columns <- grep("^cum\\.(response|pred)", names(impact$series))
  expect_equal(impact$series[-(18 : 20), - exclude.columns],
               impact.na.end$series[-(18 : 20), - exclude.columns],
               tolerance = 1e-5)
  expect_equal(impact$series[-(12 : 15), - exclude.columns],
               impact.na.middle$series[-(12 : 15), - exclude.columns],
               tolerance = 1e-5)
  expect_equal(impact$summary, impact.na.end$summary, tolerance = 1e-5)
  expect_equal(impact$summary, impact.na.middle$summary, tolerance = 1e-5)
})

test_that("CausalImpact.RunWithData.StandardizeData", {
  # Test with/without <standardize.data>
  set.seed(1)
  data <- rnorm(20, mean = 100, sd = 10)
  pre.period <- c(1, 10)
  post.period <- c(11, 20)
  model.args <- list(niter = 10000, standardize.data = FALSE)
  impact1 <- CausalImpact(data, pre.period, post.period, model.args)
  model.args <- list(niter = 10000, standardize.data = TRUE)
  impact2 <- CausalImpact(data, pre.period, post.period, model.args)
  expect_equal(impact1$series$response, zoo(data))
  expect_equal(impact2$series$response, zoo(data))
  expect_equal(impact1$series$response, impact2$series$response,
              tolerance = 0.001)
  expect_equal(impact1$series, impact2$series, tolerance = 0.1)

  # Recover ground truth (generative model with 1 covariate)
  # with/without <standardize.data>
  set.seed(1)
  n <- 500
  x1 <- sin(1:n / 100) + rnorm(n, 0, 0.1)
  w <- rnorm(n, 0, 0.1)
  beta <- c(5, 3)
  y <- beta[1] + beta[2] * x1 + w
  pre.period <- c(1, 250)
  post.period <- c(251, 500)
  data <- cbind(y, x1)
  impact1 <- CausalImpact(data, pre.period, post.period,
                          model.args = list(niter = 500,
                                            standardize.data = FALSE))
  estimates1 <- colMeans(impact1$model$bsts.model$coefficients)
  expect_equal(as.vector(estimates1)[2], beta[2], tolerance = 0.05)
  impact2 <- CausalImpact(data, pre.period, post.period,
                          model.args = list(niter = 500,
                                            standardize.data = TRUE))
  estimates2 <- colMeans(impact2$model$bsts.model$coefficients)
  expect_equal(as.vector(estimates2)[2], 1, tolerance = 0.05)
})

test_that("CausalImpact.RunWithData.ShortTimeSeries", {
  # Test daily data (zoo object)
  data <- zoo(cbind(rnorm(200), rnorm(200), rnorm(200)),
              seq.Date(as.Date("2014-01-01"), as.Date("2014-01-01") + 199,
                       by = 1))
  pre.period <- as.Date(c("2014-01-01", "2014-04-10"))  # 100 days
  post.period <- as.Date(c("2014-04-11", "2014-07-19"))  # 100 days
  model.args <- list(niter = 100)
  suppressWarnings(impact <- CausalImpact(data, pre.period, post.period,
                                          model.args))
  expect_equal(time(impact$model$bsts.model$original.series), 1:200)
  expect_equal(time(impact$series), time(data))
  CallAllS3Methods(impact)

  # Test on minimal daily data
  dates <- seq(as.Date("2013-01-01"), as.Date("2013-01-04"), by = 1)
  y <- c(1, 2, 3, 5);
  x <- c(1, 2, 3, 4);
  data <- zoo(cbind(y, x), dates)
  pre.period <- as.Date(c("2013-01-01", "2013-01-03"))
  post.period <- as.Date(c("2013-01-04", "2013-01-04"))
  model.args <- list(niter = 100)
  suppressWarnings(impact <- CausalImpact(data, pre.period, post.period,
                                          model.args))
  expect_equal(impact$summary$AbsEffect[1], 1, tolerance = 0.2)

  # Test <data> where inference is aborted
  y <- c(1, 1, 1, 1);
  x <- c(1, 2, 3, 4);
  data <- zoo(cbind(y, x))
  pre.period <- c(1, 3)
  post.period <- c(4, 4)
  expect_warning(impact <- CausalImpact(data, pre.period, post.period),
                 "Aborting inference")
  expect_equal(impact$series$response, as.zoo(y))
  expect_equal(impact$series$cum.response, as.zoo(cumsum(y)))
  expect_true(all(is.na(impact$series[, -c(1, 2)])))
  expect_true(is.null(impact$summary))
  expect_true(is.null(impact$report))
  expect_output(print(impact), "(Inference aborted)", fixed = TRUE)
  expect_output(print(impact, "report"), "(Report empty)", fixed = TRUE)
  expect_error(plot(impact), "cannot create plot")
})

test_that("CausalImpact.RunWithData.LargeIntegerInput", {
  # Creates an input time series with large integer values, tests that
  # CausalImpact processes them without integer overflow.
  set.seed(1)
  x1 <- 100 + arima.sim(model = list(ar = 0.999), n = 100)
  y <- 1.2 * x1 + rnorm(100)
  y[71:100] <- y[71:100] + 10
  data <- cbind(as.integer(1e6 * y), as.integer(1e6 * x1))
  pre.period <- c(1, 70)
  post.period <- c(71, 100)
  set.seed(42)
  expect_error(impact <- CausalImpact(data, pre.period, post.period), NA)

  # Rescales the time series to smaller values, refits the model and tests that
  # both model outcomes are identical (up to numerical imprecisions).
  rescaled.data <- data / 1e6
  set.seed(42)
  rescaled.impact <- CausalImpact(rescaled.data, pre.period, post.period)
  original.summary <- impact$summary
  rescaled.summary <- rescaled.impact$summary
  scaled.columns <- c("Actual", "Pred", "Pred.lower", "Pred.upper", "Pred.sd",
                      "AbsEffect", "AbsEffect.lower", "AbsEffect.upper",
                      "AbsEffect.sd")
  unscaled.columns <- c("RelEffect", "RelEffect.lower", "RelEffect.upper",
                        "RelEffect.sd", "alpha", "p")
  expect_equal(original.summary[, scaled.columns] / 1e6,
               rescaled.summary[, scaled.columns],
               tolerance = 0.01)
  expect_equal(original.summary[, unscaled.columns],
               rescaled.summary[, unscaled.columns],
               tolerance = 0.01)
})

test_that("CausalImpact.RunWithData.MissingTimePoint", {
  set.seed(1)
  series <- zoo(data.frame(y = rnorm(20), x = rnorm(20)),
                seq.Date(as.Date("2017-01-01"), by = 1, length.out = 20))
  pre.period <- as.Date(c("2017-01-01", "2017-01-15"))
  post.period <- as.Date(c("2017-01-16", "2017-01-20"))
  model.args <- list(niter = 100)

  # Missing in pre-period
  impact <- CausalImpact(series[-10, ], pre.period, post.period, model.args)
  indices <- time(impact$series)
  expect_equal(indices, time(series)[-10])

  # Missing in post-period
  impact <- CausalImpact(series[-17, ], pre.period, post.period, model.args)
  indices <- time(impact$series)
  expect_equal(indices, time(series)[-17])
})

test_that("CausalImpact.RunWithBstsModel", {

  # Test on a healthy bsts object
  y <- y.orig <- rnorm(200)
  post.period.response <- y.orig[101 : 200]
  y[101 : 200] <- NA
  X <- cbind(rnorm(200), rnorm(200))
  ss <- AddLocalLinearTrend(list(), y)
  bsts.model <- bsts(y ~ X, ss, niter = 100, ping = 0)
  impact <- CausalImpact(bsts.model = bsts.model,
                         post.period.response = post.period.response)
  expect_equal(names(impact), c("series", "summary", "report", "model"))
  expect_equal(names(impact$series), .expected.series.columns)
  expect_equal(nrow(impact$series), length(y))
  expect_equal(time(impact$series), 1:length(y))
  expect_equal(impact$model$pre.period, c(1, 100))
  expect_equal(impact$model$post.period, c(101, 200))
  CallAllS3Methods(impact)

  # Test on a bsts object that has been fitted on a zoo object with daily data
  y <- zoo(rnorm(10), seq.Date(as.Date("2014-01-01"), by = 1, length = 10))
  X <- as.vector(y) + rnorm(length(y))
  y[6 : 10] <- NA
  ss <- AddLocalLinearTrend(list(), y)
  bsts.model <- bsts(y ~ X, ss, niter = 100, ping = 0)
  impact <- CausalImpact(bsts.model = bsts.model,
                         post.period.response = rnorm(5))
  expect_equal(time(impact$series), time(y))
  expect_equal(impact$series$response[1 : 5], y[1 : 5])
  expect_false(anyNA(impact$series$response))

  # Test on a bsts object that has been fitted on data with NA response values
  y <- y.orig <- rnorm(200)
  y[3:5] <- NA
  y.orig[3:5] <- NA
  post.period.response <- y.orig[101 : 200]
  y[101 : 200] <- NA
  X <- cbind(rnorm(200), rnorm(200))
  ss <- AddLocalLinearTrend(list(), y)
  bsts.model <- bsts(y ~ X, ss, niter = 100, ping = 0)
  impact <- CausalImpact(bsts.model = bsts.model,
                         post.period.response = post.period.response)
  expect_equal(nrow(impact$series), length(y))
  expect_equal(time(impact$series), 1:length(y))
  expect_equal(impact$model$pre.period, c(1, 100))
  expect_equal(impact$model$post.period, c(101, 200))
  CallAllS3Methods(impact)
  #
  # Check that data points 3..5 are NA in the right columns in impact$series
  unaffected.cols <- c("point.pred", "point.pred.lower", "point.pred.upper")
  for (na.col in unaffected.cols) {
    expect_false(anyNA(as.data.frame(impact$series)[[na.col]]))
  }
  for (na.col in setdiff(names(impact$series), unaffected.cols)) {
    expect_true(all(is.na(as.data.frame(impact$series)[[na.col]][3:5])))
    expect_false(anyNA(as.data.frame(impact$series)[[na.col]][-c(3:5)]))
  }

  # Test bsts.model that has been fitted on data not conforming to the usual
  # pre/post scheme
  bad.y <- list(c(1, 2, 3, 4, 5, 6),
                c(1, 2, 3, NA, NA, 4, 5, 6),
                c(1, NA, 2, NA, 3))
  corresponding.post.period.response <- list(c(4, 5, 6),
                                             c(2, 3, 4, 5, 6),
                                             c(2, 3))
  for (i in 1:length(bad.y)) {
    y <- bad.y[[i]]
    post.period.response <- corresponding.post.period.response[[i]]
    X <- rnorm(length(y))
    ss <- AddLocalLinearTrend(list(), y)
    bsts.model <- bsts(y ~ X, ss, niter = 100, ping = 0)
    expect_error(CausalImpact(bsts.model = bsts.model,
                                post.period.response = post.period.response))
  }

  # Test <post.period.response> that is inconsistent with <bsts.model>
  # Here, post.period.response should contain exactly 100 values.
  bad.post.period.response <- list(1, rep(NA, 100), rep(as.numeric(NA), 100),
                                   c(rnorm(99), NA))
  invisible(lapply(bad.post.period.response, function(post.period.response) {
    expect_error(CausalImpact(NULL, NULL, NULL, NULL, bsts.model,
                              post.period.response, alpha))
  }))
})

test_that("CausalImpact.RunWithMaxFlips", {

  max.flips <- 10

  # Create a dataset that with 200 columns/controls
  data <- data.frame(matrix(rnorm(200 * 200), ncol = 200))

  set.seed(1)
  pre.period <- c(1, 100)
  post.period <- c(101, 200)
  model.args <- list(niter = 100, max.flips = max.flips)

  suppressWarnings(impact <- CausalImpact(data, pre.period, post.period,
                                          model.args))

  expect_equal(impact$model$bsts.model$prior$max.flips, max.flips)
})

test_that("PrintSummary", {
  PrintSummary <- CausalImpact:::PrintSummary

  set.seed(1)
  data <- zoo(cbind(c(rnorm(100), rnorm(100) + 1), rnorm(200), rnorm(200)))
  pre.period <- c(1, 100)
  post.period <- c(101, 200)
  model.args <- list(niter = 100)
  suppressWarnings(impact <- CausalImpact(data, pre.period, post.period,
                                          model.args))
  expect_output(PrintSummary(impact), "\\([0-9]+\\.[0-9]{2}\\)")
  expect_output(PrintSummary(impact, digits = 0), "\\([0-9]+\\)")
  expect_output(PrintSummary(impact, digits = 10), "\\([0-9]+\\.[0-9]{10}\\)")
})

test_that("PrintReport", {
  PrintReport <- CausalImpact:::PrintReport

  # Test invalid input
  expect_error(PrintReport(NULL), "CausalImpact")
  impact <- list(model = list(report = c("foo", "bar")))
  expect_error(PrintReport(impact), "CausalImpact")

  # Test valid input without a report
  impact <- list(model = list(report = "foo"))
  class(impact) <- "CausalImpact"
  expect_output(PrintReport(impact), "(Report empty)", fixed = TRUE)
  impact <- list(model = list(report = c("Foo.", "Bar.")))
  class(impact) <- "CausalImpact"
  expect_output(PrintReport(impact), "(Report empty)", fixed = TRUE)

  # Test manually specified precision on real CausalImpact object
  set.seed(1)
  data <- zoo(cbind(c(rnorm(100), rnorm(100) + 1), rnorm(200), rnorm(200)))
  pre.period <- c(1, 100)
  post.period <- c(101, 200)
  model.args <- list(niter = 100)
  suppressWarnings(impact <- CausalImpact(data, pre.period, post.period,
                                          model.args))
  # Regular expressions are testing for credible intervals in the form
  # "[<lower>, <upper>]", where <lower> and <upper> have to have the numerical
  # precision specified by the argument "digits" of PrintReport().
  # Test default precision of 2 digits after the decimal mark:
  expect_output(PrintReport(impact),
                "\\[[-]?[0-9]+\\.[0-9]{2}, [-]?[0-9]+\\.[0-9]{2}\\]")
  # Test numbers rounded to integers:
  expect_output(PrintReport(impact, digits = 0), "\\[[-]?[0-9]+, [-]?[0-9]+\\]")
  # Test numbers rounded to 5 digits after the decimal mark:
  expect_output(PrintReport(impact, digits = 5),
                "\\[[-]?[0-9]+\\.[0-9]{5}, [-]?[0-9]+\\.[0-9]{5}\\]")
})

test_that("AsCausalImpact", {
  # Test that <as.CausalImpact> is exported and visible.
  expect_true(is.function(as.CausalImpact))

  # Test that <as.CausalImpact> has a method (the default one) assigned
  expect_equal(as.character(methods(as.CausalImpact)),
               "as.CausalImpact.default")

  # Test that <as.CausalImpact.default> is called on a NULL object.
  expect_error(as.CausalImpact(NULL), "NULL")
})
