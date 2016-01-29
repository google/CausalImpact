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

# ------------------------------------------------------------------------------
# Unit tests for impact_analysis.R.
#
# Authors: kbrodersen@google.com (Kay H. Brodersen)
#          gallusser@google.com (Fabian Gallusser)

.expected.series.columns <-
    c("response", "cum.response",
      "point.pred", "point.pred.lower", "point.pred.upper",
      "cum.pred", "cum.pred.lower", "cum.pred.upper",
      "point.effect", "point.effect.lower", "point.effect.upper",
      "cum.effect", "cum.effect.lower", "cum.effect.upper")

# ------------------------------------------------------------------------------
CallAllS3Methods <- function(impact) {
  # This function tests the various object methods that are implemented in
  # impact_analysis.R for objects of class 'CausalImpact'.

  # If summary(impact, "foo")) does not generate an exception as expected, this
  # is probably because summary.default() is called rather than
  # summary.CausalImpact(). Make sure <impact> is of class <CausalImpact>, and
  # make sure summary.CausalImpact() is declared as an S3method export in the
  # NAMESPACE file.

  summary(impact)
  summary(impact, output = "summary")
  summary(impact, output = "report")
  summary(impact, "summary")
  summary(impact, "report")
  summary(impact, "s")
  checkException(summary(impact, "foo"))
  print(impact)
  print(impact, "summary")
  print(impact, "report")
  print(impact, "s")
  checkException(print(impact, "foo"))
  plot(impact)
  q <- plot(impact)
  plot(q)
}

# ------------------------------------------------------------------------------
TestFormatInputForCausalImpact <- function() {
  FormatInputForCausalImpact <- CausalImpact:::FormatInputForCausalImpact

  # Test missing input
  checkException(FormatInputForCausalImpact())

  # Specify some healthy input variables
  data <- zoo::zoo(data.frame(y = rnorm(200), x1 = rnorm(200), x2 = rnorm(200)))
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
  checkEquals(result[-4], expected[-4])
  checkEquals(result$model.args$niter, model.args$niter)

  # Test bsts.model input (usage scenario 2)
  expected <- list(data = NULL, pre.period = NULL,
                   post.period = NULL, model.args = NULL,
                   bsts.model = bsts.model,
                   post.period.response = post.period.response, alpha = alpha)
  checked <- FormatInputForCausalImpact(NULL, NULL, NULL, NULL,
                                        bsts.model, post.period.response, alpha)
  checkEquals(checked[-4], expected[-4])

  # Test inconsistent input (must not provide both data and bsts.model)
  checkException(FormatInputForCausalImpact(data, pre.period, post.period,
                                            model.args, bsts.model,
                                            post.period.response, alpha))

  # Test that <data> is converted to zoo
  expected.data <- zoo::zoo(c(10, 20, 30, 40), c(1, 2, 3, 4))
  dim(expected.data) <- c(4, 1)
  funny.data <- list(zoo::zoo(c(10, 20, 30, 40)),
                     zoo::zoo(c(10, 20, 30, 40), c(1, 2, 3, 4)),
                     c(10, 20, 30, 40),
                     c(10L, 20L, 30L, 40L),
                     matrix(c(10, 20, 30, 40)))
  lapply(funny.data, function(data) {
    checked <- FormatInputForCausalImpact(data, c(1, 3), c(4, 4),
                                          model.args, NULL, NULL, alpha)
    checkEquals(checked$data, expected.data)
  })

  # Test data frame input
  data <- data.frame(y = c(10, 20, 30, 40))
  expected.data <- zoo::as.zoo(data)
  checked <- FormatInputForCausalImpact(data, c(1, 3), c(4, 4),
                                        model.args, NULL, NULL, alpha)
  checkEquals(checked$data, expected.data)

  # Test bad <data>
  bad.data <- list(NULL, NA, as.numeric(NA), "foo", list(1, 2, 3, 4))
  lapply(bad.data, function(data) {
    checkException(FormatInputForCausalImpact(data, c(1, 3), c(4, 4),
                                              model.args, NULL, NULL, alpha))
  })

  # Test bad <pre.period>
  bad.pre.period <- list(NULL, 1, c(1, 2, 3), c(NA, 2), NA,
                         as.Date(c("2011-01-01", "2011-12-31")))
  lapply(bad.pre.period, function(pre.period) {
    checkException(FormatInputForCausalImpact(data, pre.period, post.period,
                                              model.args, NULL, NULL, alpha)) })

  # Test bad <post.period>
  bad.post.period <- list(NULL, 1, c(1, 2, 3), c(NA, 2), NA,
                          as.Date(c("2011-01-01", "2011-12-31")))
  lapply(bad.post.period, function(post.period) {
    checkException(FormatInputForCausalImpact(data, pre.period, post.period,
                                              model.args, NULL, NULL, alpha)) })

  # Test bad combination of <pre.period> and <post.period>
  checkException(FormatInputForCausalImpact(data, c(1, 5), c(3, 10), model.args,
                                            NULL, NULL, alpha))

  # Test what happens when pre.period/post.period has a different class than
  # the timestamps in <data>
  bad.data <- zoo::zoo(c(1, 2, 3, 4), as.Date(c("2014-01-01", "2014-01-02",
                                           "2014-01-03", "2014-01-04")))
  bad.pre.period <- c(1, 3)  # numeric
  bad.post.period <- c(4, 4)
  checkException(FormatInputForCausalImpact(bad.data,
                                            bad.pre.period, bad.post.period,
                                            model.args, NULL, NULL, alpha))
  bad.pre.period <- c(1L, 3L)  # integer
  bad.post.period <- c(4L, 4L)
  checkException(FormatInputForCausalImpact(bad.data,
                                            bad.pre.period, bad.post.period,
                                            model.args, NULL, NULL, alpha))
  ok.data <- zoo::zoo(c(1, 2, 3, 4))
  ok.pre.period <- c(1L, 3L)
  ok.post.period <- c(4L, 4L)
  FormatInputForCausalImpact(ok.data, ok.pre.period, ok.post.period,
                             model.args, NULL, NULL, alpha)
  ok.data <- zoo::zoo(c(1, 2, 3, 4), c(1, 2, 3, 4))
  ok.pre.period <- c(1, 3)
  ok.post.period <- c(4, 4)
  FormatInputForCausalImpact(ok.data, ok.pre.period, ok.post.period,
                             model.args, NULL, NULL, alpha)
  ok.data <- zoo::zoo(c(1, 2, 3, 4), c(1, 2, 3, 4))
  ok.pre.period <- c(1L, 3L)  # we'll convert integer to numeric
  ok.post.period <- c(4L, 4L)
  FormatInputForCausalImpact(ok.data, ok.pre.period, ok.post.period,
                             model.args, NULL, NULL, alpha)
  ok.data <- zoo::zoo(c(1, 2, 3, 4))
  ok.pre.period <- c(1, 3)  # we'll convert numeric to integer
  ok.post.period <- c(4, 4)
  FormatInputForCausalImpact(ok.data, ok.pre.period, ok.post.period,
                             model.args, NULL, NULL, alpha)

  # Test bad <model.args>
  bad.model.args <- list(1000, "niter = 1000")
  lapply(bad.model.args, function(model.args) {
    checkException(FormatInputForCausalImpact(data, pre.period, post.period,
                                              model.args, NULL, NULL, alpha)) })

  # Test bad <standardize.data>
  bad.standardize.data <- list(NA, as.numeric(NA), 123, "foo", c(TRUE, FALSE))
  lapply(bad.standardize.data, function(standardize.data) {
    checkException(FormatInputForCausalImpact(data, pre.period, post.period,
                                              list(standardize.data =
                                                   standardize.data),
                                              NULL, NULL, alpha)) })

  # Test bad <bsts.model>
  bad.bsts.model <- list(NULL, NA, 1, c(1, 2, 3))
  lapply(bad.bsts.model, function(bsts.model) {
    checkException(FormatInputForCausalImpact(NULL, NULL, NULL, NULL,
                                              bsts.model, post.period.response,
                                              alpha)) })

  # Test bad <post.period.response>
  # (Note that consistency with bsts.model is not tested in
  # FormatInputForCausalImpact().)
  bad.post.period.response <- list(NULL, as.Date("2011-01-01"),
                                   data.frame(x = c(1, 2, 3)), TRUE)
  lapply(bad.post.period.response, function(post.period.response) {
    checkException(FormatInputForCausalImpact(NULL, NULL, NULL, NULL,
                                              bsts.model, post.period.response,
                                              alpha)) })

  # Test bad <alpha>
  bad.alpha <- list(NULL, NA, as.numeric(NA), -1, 0, 1, c(0.8, 0.9), "0.1")
  lapply(bad.alpha, function(alpha) {
    checkException(FormatInputForCausalImpact(data, pre.period, post.period,
                                              model.args, NULL, NULL, alpha)) })
}

# ------------------------------------------------------------------------------
TestCausalImpact.RunWithData <- function() {

  # Test missing input
  checkException(CausalImpact())

  # Test anonymous zoo series
  set.seed(1)
  data <- zoo::zoo(cbind(rnorm(200), rnorm(200), rnorm(200)))
  pre.period <- c(1, 100)
  post.period <- c(101, 200)
  model.args <- list(niter = 100)
  impact <- CausalImpact(data, pre.period, post.period, model.args)
  checkEquals(names(impact), c("series", "summary", "report", "model"))
  checkEquals(names(impact$series), .expected.series.columns)
  checkEquals(nrow(impact$series), nrow(data))
  checkEquals(time(impact$series), time(data))
  CallAllS3Methods(impact)

  # Test other data formats
  set.seed(1)
  pre.period <- c(1, 100)
  post.period <- c(101, 200)
  model.args <- list(niter = 100)
  #
  # Vector
  data <- rnorm(200)
  impact <- CausalImpact(data, pre.period, post.period, model.args)
  checkEquals(names(impact$series)[1], "response")
  #
  # Named zoo series
  data <- zoo::zoo(cbind(rnorm(200), rnorm(200), rnorm(200)))
  names(data) <- c("a", "b", "c")
  impact <- CausalImpact(data, pre.period, post.period, model.args)
  checkEquals(names(impact$series)[1], "response")
  #
  # Zoo series with only 1 variable
  data <- rnorm(200)
  impact <- CausalImpact(data, pre.period, post.period, model.args)
  checkEquals(names(impact$series)[1], "response")
  #
  # Data frame
  data <- data.frame(a = rnorm(200), b = rnorm(200), c = rnorm(200))
  impact <- CausalImpact(data, pre.period, post.period, model.args)
  checkEquals(names(impact$series)[1], "response")
  #
  # Data frame with only 1 variable
  data <- data.frame(a = rnorm(200))
  impact <- CausalImpact(data, pre.period, post.period, model.args)
  checkEquals(names(impact$series)[1], "response")
  #
  # Matrix
  data <- matrix(rnorm(600), ncol = 3)
  impact <- CausalImpact(data, pre.period, post.period, model.args)
  checkEquals(names(impact$series)[1], "response")
  #
  # Matrix with only 1 column
  data <- matrix(rnorm(600), ncol = 1)
  impact <- CausalImpact(data, pre.period, post.period, model.args)
  checkEquals(names(impact$series)[1], "response")

  # Test missing data (NA) in pre-period response variable
  set.seed(1)
  data <- zoo::zoo(cbind(rnorm(200), rnorm(200), rnorm(200)))
  data[3:5, 1] <- NA
  pre.period <- c(1, 100)
  post.period <- c(101, 200)
  model.args <- list(niter = 100)
  impact <- CausalImpact(data, pre.period, post.period, model.args)
  checkEquals(nrow(impact$series), nrow(data))
  checkEquals(time(impact$series), time(data))
  unaffected.cols <- c("point.pred", "point.pred.lower", "point.pred.upper")
  for (na.col in unaffected.cols) {
    checkTrue(all(!is.na(as.data.frame(impact$series)[[na.col]])))
  }
  for (na.col in setdiff(names(impact$series), unaffected.cols)) {
    checkTrue(all(is.na(as.data.frame(impact$series)[[na.col]][3:5])))
    checkTrue(all(!is.na(as.data.frame(impact$series)[[na.col]][-c(3:5)])))
  }
  CallAllS3Methods(impact)

  # Test pre-period that does not begin at the beginning
  set.seed(1)
  data <- rnorm(20, mean = 100, sd = 10)
  pre.period <- c(3, 10)
  post.period <- c(11, 20)
  model.args <- list(niter = 100)
  impact <- CausalImpact(data, pre.period, post.period, model.args)
  checkTrue(all(is.na(impact$series[1 : (pre.period[1] - 1), -c(1, 2)])))
  checkEquals(impact$series$response, zoo::zoo(data))

  # Test post-period that does not last until the end of the data
  set.seed(1)
  data <- rnorm(20, mean = 100, sd = 10)
  pre.period <- c(1, 10)
  post.period <- c(11, 18)
  model.args <- list(niter = 100)
  impact <- CausalImpact(data, pre.period, post.period, model.args)
  checkTrue(all(is.na(window(impact$series,
                             start = post.period[2] + 1)[, -1])))
  checkEquals(impact$series$response, zoo::zoo(data))

  # Test with/without <standardize.data>
  set.seed(1)
  data <- rnorm(20, mean = 100, sd = 10)
  pre.period <- c(1, 10)
  post.period <- c(11, 20)
  model.args <- list(niter = 10000, standardize.data = FALSE)
  impact1 <- CausalImpact(data, pre.period, post.period, model.args)
  model.args <- list(niter = 10000, standardize.data = TRUE)
  impact2 <- CausalImpact(data, pre.period, post.period, model.args)
  checkEquals(impact1$series$response, zoo::zoo(data))
  checkEquals(impact2$series$response, zoo::zoo(data))
  checkEquals(impact1$series$response, impact2$series$response,
              tolerance = 0.001)
  checkEquals(impact1$series, impact2$series, tolerance = 0.1)

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
  checkEquals(as.vector(estimates1)[2], beta[2], tolerance = 0.05)
  impact2 <- CausalImpact(data, pre.period, post.period,
                          model.args = list(niter = 500,
                                            standardize.data = TRUE))
  estimates2 <- colMeans(impact2$model$bsts.model$coefficients)
  checkEquals(as.vector(estimates2)[2], 1, tolerance = 0.05)

  # Test daily data (zoo object)
  data <- zoo::zoo(cbind(rnorm(200), rnorm(200), rnorm(200)),
              seq.Date(as.Date("2014-01-01"), as.Date("2014-01-01") + 199,
                       by = 1))
  pre.period <- as.Date(c("2014-01-01", "2014-04-10"))  # 100 days
  post.period <- as.Date(c("2014-04-11", "2014-07-19"))  # 100 days
  model.args <- list(niter = 100)
  impact <- CausalImpact(data, pre.period, post.period, model.args)
  checkEquals(time(impact$model$bsts.model$original.series), time(data))
  checkEquals(time(impact$series), time(data))
  CallAllS3Methods(impact)

  # Test on minimal daily data
  dates <- seq(as.Date("2013-01-01"), as.Date("2013-01-04"), by = 1)
  y <- c(1, 2, 3, 5);
  x <- c(1, 2, 3, 4);
  data <- zoo::zoo(cbind(y, x), dates)
  pre.period <- as.Date(c("2013-01-01", "2013-01-03"))
  post.period <- as.Date(c("2013-01-04", "2013-01-04"))
  model.args <- list(niter = 100)
  impact <- CausalImpact(data, pre.period, post.period, model.args)
  checkEquals(impact$summary$AbsEffect[1], 1, tolerance = 0.2)

  # Test <data> where inference is aborted
  y <- c(1, 1, 1, 1);
  x <- c(1, 2, 3, 4);
  data <- zoo::zoo(cbind(y, x))
  pre.period <- c(1, 3)
  post.period <- c(4, 4)
  impact <- CausalImpact(data, pre.period, post.period)
  checkEquals(impact$series$response, zoo::as.zoo(y))
  checkEquals(impact$series$cum.response, zoo::as.zoo(cumsum(y)))
  checkTrue(all(is.na(impact$series[, -c(1, 2)])))
  checkTrue(is.null(impact$summary))
  checkTrue(is.null(impact$report))
  print(impact)
  print(impact, "report")
  checkException(plot(impact))
}

# ------------------------------------------------------------------------------
TestCausalImpact.RunWithBstsModel <- function() {

  # Test on a healthy bsts object
  y <- y.orig <- rnorm(200)
  post.period.response <- y.orig[101 : 200]
  y[101 : 200] <- NA
  X <- cbind(rnorm(200), rnorm(200))
  ss <- AddLocalLinearTrend(list(), y)
  bsts.model <- bsts::bsts(y ~ X, ss, niter = 100, ping = 0)
  impact <- CausalImpact(bsts.model = bsts.model,
                         post.period.response = post.period.response)
  checkEquals(names(impact), c("series", "summary", "report", "model"))
  checkEquals(names(impact$series), .expected.series.columns)
  checkEquals(nrow(impact$series), length(y))
  checkEquals(time(impact$series), 1:length(y))
  checkEquals(impact$model$pre.period, c(1, 100))
  checkEquals(impact$model$post.period, c(101, 200))
  CallAllS3Methods(impact)

  # Test on a bsts object that has been fitted on a zoo object with daily data
  y <- zoo::zoo(rnorm(10), seq.Date(as.Date("2014-01-01"), by = 1, length = 10))
  X <- as.vector(y) + rnorm(length(y))
  y[6 : 10] <- NA
  ss <- AddLocalLinearTrend(list(), y)
  bsts.model <- bsts::bsts(y ~ X, ss, niter = 100, ping = 0)
  impact <- CausalImpact(bsts.model = bsts.model,
                         post.period.response = rnorm(5))
  checkEquals(time(impact$series), time(y))
  checkEquals(impact$series$response[1 : 5], y[1 : 5])
  checkTrue(all(!is.na(impact$series$response)))

  # Test on a bsts object that has been fitted on data with NA response values
  y <- y.orig <- rnorm(200)
  y[3:5] <- NA
  y.orig[3:5] <- NA
  post.period.response <- y.orig[101 : 200]
  y[101 : 200] <- NA
  X <- cbind(rnorm(200), rnorm(200))
  ss <- AddLocalLinearTrend(list(), y)
  bsts.model <- bsts::bsts(y ~ X, ss, niter = 100, ping = 0)
  impact <- CausalImpact(bsts.model = bsts.model,
                         post.period.response = post.period.response)
  checkEquals(nrow(impact$series), length(y))
  checkEquals(time(impact$series), 1:length(y))
  checkEquals(impact$model$pre.period, c(1, 100))
  checkEquals(impact$model$post.period, c(101, 200))
  CallAllS3Methods(impact)
  #
  # Check that data points 3..5 are NA in the right columns in impact$series
  unaffected.cols <- c("point.pred", "point.pred.lower", "point.pred.upper")
  for (na.col in unaffected.cols) {
    checkTrue(all(!is.na(as.data.frame(impact$series)[[na.col]])))
  }
  for (na.col in setdiff(names(impact$series), unaffected.cols)) {
    checkTrue(all(is.na(as.data.frame(impact$series)[[na.col]][3:5])))
    checkTrue(all(!is.na(as.data.frame(impact$series)[[na.col]][-c(3:5)])))
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
    bsts.model <- bsts::bsts(y ~ X, ss, niter = 100, ping = 0)
    checkException(CausalImpact(bsts.model = bsts.model,
                                post.period.response = post.period.response))
  }

  # Test <post.period.response> that is inconsistent with <bsts.model>
  # Here, post.period.response should contain exactly 100 values.
  bad.post.period.response <- list(1, rep(NA, 100), rep(as.numeric(NA), 100),
                                   c(rnorm(99), NA))
  lapply(bad.post.period.response, function(post.period.response) {
    checkException(CausalImpact(NULL, NULL, NULL, NULL,
                                bsts.model, post.period.response, alpha))
  })
}

# ------------------------------------------------------------------------------
TestPrintSummary <- function() {
  PrintSummary <- CausalImpact:::PrintSummary

  set.seed(1)
  data <- zoo::zoo(cbind(c(rnorm(100), rnorm(100) + 1), rnorm(200), rnorm(200)))
  pre.period <- c(1, 100)
  post.period <- c(101, 200)
  model.args <- list(niter = 100)
  impact <- CausalImpact(data, pre.period, post.period, model.args)
  PrintSummary(impact)
  PrintSummary(impact, digits = 0)
  PrintSummary(impact, digits = 10)
}

# ------------------------------------------------------------------------------
TestPrintReport <- function() {
  PrintReport <- CausalImpact:::PrintReport

  # Check invalid input
  checkException(PrintReport(NULL))
  impact <- list(model = list(report = c("foo", "bar")))
  checkException(PrintReport(impact))

  # Check valid input
  impact <- list(model = list(report = "foo"))
  class(impact) <- "CausalImpact"
  PrintReport(impact)
  impact <- list(model = list(report = c("Foo.", "Bar.")))
  class(impact) <- "CausalImpact"
  PrintReport(impact)
}
