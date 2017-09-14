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

testthat::context("Unit tests for impact_misc.R")

# Authors: kbrodersen@google.com (Kay Brodersen)
#          gallusser@google.com (Fabian Gallusser)
#          alhauser@google.com (Alain Hauser)

CreateDummySeries <- function() {
  # Creates a dummy series for testing: 3 years of data, single variable.
  #
  # Returns:
  #   a zoo object with a single series

  set.seed(42)
  dates <- seq.Date(as.Date("2011-01-01"), as.Date("2013-12-31"), by = 1)
  data <- zoo(rnorm(length(dates), dates))
  data[10] <- 5
  data[20] <- -5
  return(data)
}

test_that("is.wholenumber", {
  is.wholenumber <- CausalImpact:::is.wholenumber

  # Test empty input
  expect_error(is.wholenumber(), "missing")

  # Test various standard cases
  expect_error(is.wholenumber("a"), "numeric")
  expect_equal(is.wholenumber(c(-1, 0, 1, 2, -1.1, 0.1, 1.1)),
               c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(is.wholenumber(NA), NA)

  # Test documentation example
  expect_equal(is.wholenumber(c(1, 1.0, 1.2)), c(TRUE, TRUE, FALSE))

  # Test different tolerances
  expect_true(is.wholenumber(3.14, tolerance = 0.2))
  expect_false(is.wholenumber(3.14, tolerance = 0.1))
})

test_that("cumsum.na.rm", {
  cumsum.na.rm <- CausalImpact:::cumsum.na.rm

  # Test empty input
  expect_error(is.wholenumber())

  # Test healthy input
  expect_equal(cumsum.na.rm(c(1, NA, 2)), c(1, NA, 3))
  expect_equal(cumsum.na.rm(c(NA, 1, 2)), c(NA, 1, 3))
  expect_equal(cumsum.na.rm(c(1, 2, NA)), c(1, 3, NA))
  expect_equal(cumsum.na.rm(c(1, 2, 3, 4)), cumsum(c(1, 2, 3, 4)))

  # Test degenerate input
  expect_equal(cumsum.na.rm(NULL), NULL)
  expect_equal(cumsum.na.rm(c(NA, NA, NA)), as.numeric(c(NA, NA, NA)))
  expect_equal(cumsum.na.rm(c(NA, NA)), as.numeric(c(NA, NA)))
  expect_equal(cumsum.na.rm(c(0, NA, NA, 0)), c(0, NA, NA, 0))
})

test_that("is.numerically.equal", {
  is.numerically.equal <- CausalImpact:::is.numerically.equal

  # Test invalid input.
  expect_error(is.numerically.equal(), "missing")
  expect_error(is.numerically.equal("x", 3), "numeric")
  expect_error(is.numerically.equal(1, c(2, 3)), "scalar")
  expect_error(is.numerically.equal(1, 2, "tol"), "numeric")
  expect_error(is.numerically.equal(1, 2, -1), "tolerance")

  # Test valid input with two values being 'numerically equal' within the
  # specified tolerance.
  expect_true(is.numerically.equal(0, 0))
  expect_true(is.numerically.equal(0, 0, tolerance = 1e-20))
  expect_true(is.numerically.equal(1, 1))
  expect_true(is.numerically.equal(-1, -1 + 1e-9))
  expect_true(is.numerically.equal(1, -1, tolerance = 2.1))
  expect_true(is.numerically.equal(0, 1, tolerance = 1.01))

  # Test valid input with two values not being 'numerically equal' within the
  # specified tolerance.
  expect_false(is.numerically.equal(-1, -1.2))
  expect_false(is.numerically.equal(-1e-15, 1e-15))
  expect_false(is.numerically.equal(1, 1 + 1e-9, tolerance = 1e-10))
})

test_that("ParseArguments", {
  ParseArguments <- CausalImpact:::ParseArguments

  # Test missing input
  expect_error(ParseArguments())

  # Test healthy input
  args <- list(a = 10)
  defaults <- list(a = 1, b = 2)
  result <- ParseArguments(args, defaults)
  expect_equal(result, list(a = 10, b = 2))

  # Test NULL <args>
  result <- ParseArguments(NULL, list(a = 1, b = 2))
  expect_equal(result, list(a = 1, b = 2))

  # Test <args> where an individual field is NULL
  result <- ParseArguments(list(a = NULL), list(a = 1, b = 2))
  expect_equal(result, list(a = 1, b = 2))

  # Test bad input: NULL <defaults>
  expect_error(ParseArguments(NULL, NULL))

  # Test <allow.extra.args>
  result <- ParseArguments(list(c = 1), list(a = 1), allow.extra.args = TRUE)
  expect_equal(result, list(c = 1, a = 1))
  expect_error(ParseArguments(list(c = 1), list(a = 1),
                                allow.extra.args = FALSE))
})

test_that("Standardize", {
  Standardize <- CausalImpact:::Standardize

  # Test that missing input throws an error.
  expect_error(Standardize(), "missing")

  # Test that an invalid fit range throws an error.
  bad.fit.range <- list(c(1, NA_real_), 1, c(2, 1), c(-1, 1), c(1, 5))
  invisible(lapply(bad.fit.range, function(fit.range) {
    expect_error(Standardize(1 : 4, fit.range), "fit.range", fixed = TRUE)}))

  # Test the basics
  data <- c(-1, 0.1, 1, 2, NA, 3)
  result <- Standardize(data, c(1, 5))
  expect_true(is.list(result))
  expect_equal(names(result), c("y", "UnStandardize"))
  expect_equal(result$UnStandardize(result$y), data)

  # Test the maths
  expect_equal(Standardize(1 : 3)$y, c(-1, 0, 1))
  expect_equal(Standardize(1 : 5, c(1, 3))$y, c(-1, 0, 1, 2, 3))

  # Test that inputs are correctly recovered (including zoo input)
  test.data <- list(c(1), c(1, 1, 1), as.numeric(NA), c(1, NA, 3),
                    zoo(c(10, 20, 30), c(1, 2, 3)))
  lapply(test.data, function(data) {
    result <- Standardize(data)
    expect_equal(result$UnStandardize(result$y), data)
  })

  # Test bad input: matrix
  expect_error(Standardize(matrix(rnorm(10), ncol = 2)))
})

test_that("StandardizeAllVariables", {
  StandardizeAllVariables <- CausalImpact:::StandardizeAllVariables
  Standardize <- CausalImpact:::Standardize

  # Test that missing input throws an error.
  expect_error(StandardizeAllVariables(), "missing")

  # Test that an invalid fit range throws an error.
  data <- matrix(0, ncol = 3, nrow = 4)
  bad.fit.range <- list(c(1, NA_real_), 1, c(2, 1), c(-1, 1), c(1, 5))
  invisible(lapply(bad.fit.range, function(fit.range) {
    expect_error(StandardizeAllVariables(data, fit.range), "fit.range",
                 fixed = TRUE)}))

  # Test healthy input: several columns
  set.seed(1)
  data <- zoo(cbind(rnorm(100, mean = 1000, sd = 100),
                    rnorm(100, mean = 2000, sd = 200),
                    rnorm(100, mean = 3000, sd = 300)))
  result <- StandardizeAllVariables(data)
  expect_equal(length(result), 2)
  expect_equal(names(result), c("data", "UnStandardize"))
  sapply(1 : ncol(result$data), function(column) {
    expect_equal(mean(result$data[, column]), 0, tolerance = 0.0001);
    expect_equal(sd(result$data[, column]), 1, tolerance = 0.0001)
  })
  expect_equal(result$UnStandardize, Standardize(data[, 1])$UnStandardize)

  # Test that several columns are standardized correctly when fitting mean and
  # SD only over part of the rows.
  result <- StandardizeAllVariables(data, c(11, 90))
  sapply(1 : ncol(result$data), function(column) {
    expect_equal(mean(result$data[11 : 90, column]), 0, tolerance = 0.0001);
    expect_equal(sd(result$data[11 : 90, column]), 1, tolerance = 0.0001)
  })
  expect_equal(result$UnStandardize, Standardize(data[, 1])$UnStandardize)

  # Test healthy input: single series only
  set.seed(1)
  data <- zoo(rnorm(100) * 100 + 1000)
  result <- StandardizeAllVariables(data)
  expect_equal(length(result), 2)
  expect_equal(names(result), c("data", "UnStandardize"))
  expect_equal(mean(result$data), 0, tolerance = 0.0001)
  expect_equal(sd(result$data), 1, tolerance = 0.0001)
  expect_equal(result$UnStandardize, Standardize(data)$UnStandardize)

  # Test that a single series is standardized correctly when fitting mean and SD
  # only over part of the data range.
  result <- StandardizeAllVariables(data, c(11, 90))
  expect_equal(mean(result$data[11: 90]), 0, tolerance = 0.0001)
  expect_equal(sd(result$data[11 : 90]), 1, tolerance = 0.0001)
  expect_equal(result$UnStandardize, Standardize(data)$UnStandardize)
})

test_that("GetPeriodIndices.InvalidInput", {
  GetPeriodIndices <- CausalImpact:::GetPeriodIndices

  # Test missing input
  expect_error(GetPeriodIndices(), "missing")

  # Test wrong order of <period> and <times>
  expect_error(GetPeriodIndices(1:200, c(101L, 200L)), "period")

  # Test invalid times
  times <- seq.Date(as.Date("2014-01-01"), as.Date("2014-01-01") + 199, by = 1)
  bad.times <- list(NA, c(1:9, NA, 11:20), as.character(times))
  invisible(lapply(bad.times, function(times) {
    expect_error(GetPeriodIndices(c(101L, 200L), times), "times")
  }))

  # Test invalid period
  bad.period <- list(NA, 1:100, 1:3, 200, c(150, 101))
  invisible(lapply(bad.period, function(period) {
    expect_error(GetPeriodIndices(period, 1:200), "period")
  }))

  # Test inconsistent period and times
  times <- seq.Date(as.Date("2014-01-01"), as.Date("2014-01-01") + 199, by = 1)
  period <- as.Date(c("2014-04-11", "2014-07-19"))  # 100 days
  expect_error(GetPeriodIndices(c(101L, 200L), times))
  expect_error(GetPeriodIndices(period, 1:200))
  # TODO(alhauser): check for the content of the error message again once
  # assertthat produces meaningful messages under R 3.5.0; currently the error
  # message under R 3.5.0 says that the actual error message is invalid.

  # Test period that is completely outside the range of <times>:
  # - with integer time points
  expect_error(GetPeriodIndices(c(-20L, -10L), 1:200), "period")
  expect_error(GetPeriodIndices(c(201L, 210L), 1:200), "period")
  #
  # - with Date time points
  times <- seq.Date(as.Date("2014-01-01"), as.Date("2014-01-01") + 199, by = 1)
  expect_error(GetPeriodIndices(as.Date(c("2013-12-24", "2013-12-31")), times),
               "period")
  expect_error(GetPeriodIndices(as.Date(c("2014-12-24", "2014-12-31")), times),
               "period")

  # Test period that is inside the range of <times>, but so short it does not
  # touch a single time point
  expect_error(GetPeriodIndices(c(13L, 14L), 10L*(0:9)), "one data point")
  times <- seq.Date(as.Date("2015-01-01"), as.Date("2015-01-01") + 28, by = 7)
  period <- as.Date(c("2015-01-03", "2015-01-04"))
  expect_error(GetPeriodIndices(period, times), "one data point")
})

test_that("GetPeriodIndices.HealthyInput", {
  GetPeriodIndices <- CausalImpact:::GetPeriodIndices

  # Test healthy input with integer time points
  period <- c(101L, 200L)
  times <- 1:200
  result <- GetPeriodIndices(period, times)
  expect_equal(result, period)
  expect_true(is.integer(result))

  # Integer time points not starting at 1
  period <- c(101L, 200L)
  times <- 51:200
  result <- GetPeriodIndices(period, times)
  expect_equal(result, c(51, 150))
  expect_true(is.integer(result))

  # Test healthy input with Date time points
  period <- as.Date(c("2014-04-11", "2014-07-19"))  # 100 days
  times <- seq.Date(as.Date("2014-01-01"), as.Date("2014-01-01") + 199, by = 1)
  result <- GetPeriodIndices(period, times)
  expect_equal(result, c(101, 200))
  expect_true(is.integer(result))

  # Test period consisting of one single time point, for integer time points
  period <- c(21L, 21L)
  times <- 11:30
  result <- GetPeriodIndices(period, times)
  expect_equal(result, c(11, 11))

  # Test period consisting of one single time point, for Date time points
  period <- as.Date(c("2014-01-10", "2014-01-10"))
  times <- seq.Date(as.Date("2014-01-01"), as.Date("2014-01-31"), by = 1)
  result <- GetPeriodIndices(period, times)
  expect_equal(result, c(10, 10))

  # Test period going beyond the range of <times>, for integer time points
  expect_equal(GetPeriodIndices(c(1L, 20L), 11:30), c(1, 10))
  expect_equal(GetPeriodIndices(c(21L, 40L), 11:30), c(11, 20))
  expect_equal(GetPeriodIndices(c(1L, 40L), 11:30), c(1, 20))

  # Test period going beyond the range of <times>, for Date time points
  times <- seq.Date(as.Date("2015-03-11"), as.Date("2015-03-30"), by = 1)
  expect_equal(GetPeriodIndices(as.Date(c("2015-03-01", "2015-03-20")), times),
               c(1, 10))
  expect_equal(GetPeriodIndices(as.Date(c("2015-03-21", "2015-04-01")), times),
               c(11, 20))
  expect_equal(GetPeriodIndices(as.Date(c("2015-03-01", "2015-04-01")), times),
               c(1, 20))
})

test_that("InferPeriodIndicesFromData", {
  InferPeriodIndicesFromData <- CausalImpact:::InferPeriodIndicesFromData

  # Test missing input
  expect_error(InferPeriodIndicesFromData())

  # Test healthy input
  expect_equal(InferPeriodIndicesFromData(c(10, 20, 30, NA, NA, NA)),
              list(pre.period = c(1, 3), post.period = c(4, 6)))
  expect_equal(InferPeriodIndicesFromData(c(10, NA)),
              list(pre.period = c(1, 1), post.period = c(2, 2)))
  expect_equal(InferPeriodIndicesFromData(c(NA, NA, 10, 20, NA, NA)),
              list(pre.period = c(3, 4), post.period = c(5, 6)))

  # Test bad input
  expect_error(InferPeriodIndicesFromData(1))
  expect_error(InferPeriodIndicesFromData(NA))
  expect_error(InferPeriodIndicesFromData(c(1, 2, 3)))
  expect_error(InferPeriodIndicesFromData(c(NA, NA, NA)))
  expect_error(InferPeriodIndicesFromData(c(NA, NA, 1, 2, 3)))
})

test_that("PrettifyPercentage", {
  PrettifyPercentage <- CausalImpact:::PrettifyPercentage

  expect_equal(PrettifyPercentage(0.05), "+5%")
  expect_equal(PrettifyPercentage(-0.053), "-5%")
  expect_equal(PrettifyPercentage(c(0.05, 0.01)), c("+5%", "+1%"))
  expect_equal(PrettifyPercentage(0.05, 1), "+5.0%")
  expect_equal(PrettifyPercentage(0.1234, 1), "+12.3%")

  # Test documentation example
  expect_equal(PrettifyPercentage(c(-0.125, 0.2), 2), c("-12.50%", "+20.00%"))
})

test_that("PrettifyNumber", {
  PrettifyNumber <- CausalImpact:::PrettifyNumber

  # Test invalid input
  expect_error(PrettifyNumber("3.141"), "numeric")
  expect_error(PrettifyNumber(3.141, 2), "character")
  expect_error(PrettifyNumber(3.141, round.digits = -2), "round.digits",
               fixed = TRUE)
  expect_error(PrettifyNumber(123.456, letter = "foo"), "letter")

  # Test standard precision
  expect_equal(PrettifyNumber(123.456), "123.5")
  expect_equal(PrettifyNumber(123.456, letter = "K"), "0.1K")
  input <- c(0, 0.01, 0.0123, 1, -123, 12345, -1234567, 1982345670)
  output <- c("0.0", "0.01", "0.01", "1.0", "-123.0", "12.3K", "-1.2M", "2.0B")
  expect_equal(PrettifyNumber(input), output)

  # Test documentation examples
  expect_equal(PrettifyNumber(c(0.123, 123, 123456)),
               c("0.1", "123.0", "123.5K"))
  expect_equal(PrettifyNumber(3995, letter = "K", round.digits = 2), "4.00K")
  expect_equal(PrettifyNumber(1.234e-3, round.digits = 2), "0.0012")

  # Test manually specified precision
  expect_equal(PrettifyNumber(0.01, round.digits = 1), "0.01")
  expect_equal(PrettifyNumber(-0.0123, round.digits = 2), "-0.012")
  expect_equal(PrettifyNumber(123456, round.digits = 2), "123.46K")
  expect_equal(PrettifyNumber(0, round.digits = 2), "0.00")

  # Test numbers with trailing zeros
  expect_equal(PrettifyNumber(0.2, round.digits = 2), "0.20")
  expect_equal(PrettifyNumber(-0.2, round.digits = 4), "-0.2000")
  expect_equal(PrettifyNumber(2, round.digits = 2), "2.00")
  expect_equal(PrettifyNumber(-2000, round.digits = 3), "-2.000K")

  # Test non-finite input
  input <- c(NA, NaN, Inf, -Inf)
  expect_equal(PrettifyNumber(input), c("NA", "NaN", "Inf", "-Inf"))
})

test_that("IdentifyNumberAbbreviation", {
  IdentifyNumberAbbreviation <- CausalImpact:::IdentifyNumberAbbreviation

  expect_equal(IdentifyNumberAbbreviation("0.1"), "none")
  expect_equal(IdentifyNumberAbbreviation("0.1K"), "K")
  output <- c("0", "1", "123", "12.3K", "1.2M", "2B")
  letter <- c("none", "none", "none", "K", "M", "B")
  expect_equal(IdentifyNumberAbbreviation(output), letter)

  # Test documentation example
  expect_equal(IdentifyNumberAbbreviation("123.5K"), "K")
})
