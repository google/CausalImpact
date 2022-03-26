# Copyright 2014-2022 Google Inc. All rights reserved.
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

testthat::context("Unit tests for impact_plot.R")

# Author: kbrodersen@google.com (Kay Brodersen)

test_that("CreatePeriodMarkers", {
  CreatePeriodMarkers <- CausalImpact:::CreatePeriodMarkers

  # Creates a time series with integer time points, and specifies a pre- and
  # post-period that together cover all time points. Test that only one period
  # marker is created, specifying the last time point of the pre-period.
  times <- 1:50
  pre.period <- c(1L, 30L)
  post.period <- c(31L, 50L)
  markers <- CreatePeriodMarkers(pre.period, post.period, times)
  expect_equal(markers, 30)

  # Creates a time series with numeric time points, and specifies a pre- and
  # post-period that together cover all time points, but whose end points do not
  # match any time points in the series. Test that only one period marker is
  # created.
  times <- as.numeric(1:50)
  pre.period <- c(0.5, 30.2)
  post.period <- c(30.7, 50.5)
  markers <- CreatePeriodMarkers(pre.period, post.period, times)
  expect_equal(markers, 30)

  # Creates a time series with numeric time points. Specifies a pre-period that
  # starts after the beginning of the time series, a gap between pre- and
  # post-period, and a post-period that does not last until the end of the data.
  # Tests that four period markers are created.
  times <- 1:50
  pre.period <- c(11L, 20L)
  post.period <- c(31L, 40L)
  markers <- CreatePeriodMarkers(pre.period, post.period, times)
  expect_equal(markers, c(11, 20, 31, 40))

  # Creates a daily time series, and specifies a pre- and post-period that
  # together cover all time points. Test that only one period marker is created.
  times <- seq.Date(as.Date("2014-01-01"), by = 1, length.out = 50)
  # Time series runs from 2014-01-01 till 2014-02-19.
  pre.period <- as.Date(c("2014-01-01", "2014-01-30"))
  post.period <- as.Date(c("2014-01-31", "2014-02-19"))
  markers <- CreatePeriodMarkers(pre.period, post.period, times)
  expect_equal(markers, as.numeric(as.Date("2014-01-30")))

  # Creates a weekly time series, and specifies a pre- and post-period that
  # together cover all time points, but whose end points do not match any time
  # points in the series. Test that only one period marker is created.
  times <- seq.Date(as.Date("2014-01-01"), by = 7, length.out = 50)
  pre.period <- c(times[1] - 1, times[30] + 1)
  post.period <- c(times[31] - 2, times[50] + 1)
  markers <- CreatePeriodMarkers(pre.period, post.period, times)
  expect_equal(markers, as.numeric(times[30]))

  # Creates an hourly time series. Specifies a pre-period that starts after the
  # beginning of the time series, a gap between pre- and post-period, and a
  # post-period that does not last until the end of the data. Tests that four
  # period markers are created.
  times <- seq.POSIXt(strptime("2014-01-01 00:00:00",
                               format = "%Y-%m-%d %H:%M:%S"),
                      by = 3600, length.out = 48)
  # Time series runs from 2014-01-01 00:00:00 till 2014-01-02 23:00:00.
  pre.period <- as.POSIXct(strptime(c("2014-01-01 10:00:00",
                                      "2014-01-01 20:00:00"),
                                    format = "%Y-%m-%d %H:%M:%S"))
  post.period <- as.POSIXct(strptime(c("2014-01-02 10:00:00",
                                       "2014-01-02 20:00:00"),
                                     format = "%Y-%m-%d %H:%M:%S"))
  markers <- CreatePeriodMarkers(pre.period, post.period, times)
  expect_equal(markers, as.numeric(strptime(c("2014-01-01 10:00:00",
                                              "2014-01-01 20:00:00",
                                              "2014-01-02 10:00:00",
                                              "2014-01-02 20:00:00"),
                                            format = "%Y-%m-%d %H:%M:%S")))
})

test_that("CreateImpactPlot", {
  CreateImpactPlot <- CausalImpact:::CreateImpactPlot

  # Test empty input
  expect_error(CreateImpactPlot())

  # Test input with integer time indices
  x <- 1 : 20
  y <- x + rnorm(20) + c(rep(0, 10), rep(10, 10))
  data <- zoo(cbind(y, x))
  pre.period <- c(1, 10)
  post.period <- c(11, 20)
  model.args <- list(niter = 500)
  suppressWarnings(impact <- CausalImpact(data, pre.period, post.period,
                                          model.args))
  q <- CreateImpactPlot(impact)
  expect_equal(class(q), c("gg", "ggplot"))
  expect_error(suppressWarnings(plot(q)), NA)

  # Test input with Date time indices
  data <- zoo(cbind(y, x),
              seq.Date(as.Date("2014-01-01"), as.Date("2014-01-20"), by = 1))
  pre.period <- as.Date(c("2014-01-01", "2014-01-10"))
  post.period <- as.Date(c("2014-01-11", "2014-01-20"))
  suppressWarnings(impact <- CausalImpact(data, pre.period, post.period,
                                          model.args))
  q <- CreateImpactPlot(impact)
  expect_equal(class(q), c("gg", "ggplot"))
  expect_error(suppressWarnings(plot(q)), NA)

  # Test plot.CausalImpact() generic
  q1 <- CreateImpactPlot(impact)
  q2 <- plot(impact)  # dispatched to plot.CausalImpact()
  expect_equal(q1, q2, check.environment = FALSE)

  # Test plotting different metrics
  q1 <- plot(impact)
  q2 <- plot(impact, c("original", "pointwise", "cumulative"))
  q3 <- plot(impact, c("o", "point", "c"))
  expect_equal(q1, q2, check.environment = FALSE)
  # As of ggplot 2.0.0, `q1` and `q2` are still the same but `q3` is different.
  # This is because `q1` and `q2` contains:
  # > q1$plot_env$metrics
  #   original    pointwise   cumulative
  #  "original"  "pointwise" "cumulative"
  # Whereas `q3` contains:
  # > q3$plot_env$metrics
  #          o        point            c
  #  "original"  "pointwise" "cumulative"
  # So we test whether `q1` equals `q3` except for `$plot_env$metrics`.
  q3$plot_env$metrics <- q2$plot_env$metrics
  expect_equal(q1, q3, check.environment = FALSE)

  # Test different order
  q1 <- plot(impact, c("p", "c"))
  q2 <- plot(impact, c("c", "p"))
  expect_true(!isTRUE(all.equal(q1, q2, check.environment = FALSE)))
})
