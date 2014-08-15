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
# Unit tests for impact_misc.R.
#
# Authors: kbrodersen@google.com (Kay Brodersen)
#          gallusser@google.com (Fabian Gallusser)

# ------------------------------------------------------------------------------
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

# ------------------------------------------------------------------------------
TestRepmat <- function() {
  repmat <- CausalImpact:::repmat

  # Test empty input
  checkException(repmat())

  # Test various standard cases
  checkException(repmat(data.frame(x = c(1, 2, 3)), 1, 1))
  checkException(repmat(1, c(1, 2), 1))
  checkException(repmat(1, 1, c(1, 2)))
  checkEquals(repmat(1, 1, 1), as.matrix(1))
  checkEquals(repmat(1, 2, 1), rbind(1, 1))
  checkEquals(repmat(1, 1, 2), t(c(1, 1)))
  checkEquals(repmat(c(1, 2), 2, 1), rbind(c(1, 2), c(1, 2)))
  checkEquals(repmat("a", 1, 2), as.matrix(t(c("a", "a"))))
  checkEquals(repmat(NA, 1, 2), as.matrix(t(c(NA, NA))))

  # Test documentation example
  checkEquals(repmat(c(10, 20), 1, 2), as.matrix(t(c(10, 20, 10, 20))))
}

# ------------------------------------------------------------------------------
TestIsWholeNumber <- function() {
  is.wholenumber <- CausalImpact:::is.wholenumber

  # Test empty input
  checkException(is.wholenumber())

  # Test various standard cases
  checkException(is.wholenumber("a"))
  checkEquals(is.wholenumber(c(-1, 0, 1, 2, -1.1, 0.1, 1.1)),
              c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
  checkEquals(is.wholenumber(NA), NA)

  # Test documentation example
  checkEquals(is.wholenumber(c(1, 1.0, 1.2)), c(TRUE, TRUE, FALSE))
}

# ------------------------------------------------------------------------------
TestAssert <- function() {
  assert <- CausalImpact:::assert
  TryError <- CausalImpact:::TryError

  assert()
  assert(TRUE)
  assert(TRUE, 'foo')
  checkTrue(TryError(assert(FALSE))$message == "")
  checkTrue(TryError(assert(FALSE, 'foo'))$message == "foo")
}

# ------------------------------------------------------------------------------
TestParseArguments <- function() {
  ParseArguments <- CausalImpact:::ParseArguments

  # Test missing input
  checkException(ParseArguments())

  # Test healthy input
  args <- list(a = 10)
  defaults <- list(a = 1, b = 2)
  result <- ParseArguments(args, defaults)
  checkEquals(result, list(a = 10, b = 2))

  # Test NULL <args>
  result <- ParseArguments(NULL, list(a = 1, b = 2))
  checkEquals(result, list(a = 1, b = 2))

  # Test <args> where an individual field is NULL
  result <- ParseArguments(list(a = NULL), list(a = 1, b = 2))
  checkEquals(result, list(a = 1, b = 2))

  # Test bad input: NULL <defaults>
  checkException(ParseArguments(NULL, NULL))

  # Test <allow.extra.args>
  result <- ParseArguments(list(c = 1), list(a = 1), allow.extra.args = TRUE)
  checkEquals(result, list(c = 1, a = 1))
  checkException(ParseArguments(list(c = 1), list(a = 1),
                                allow.extra.args = FALSE))
}

# ------------------------------------------------------------------------------
TestStandardize <- function() {
  Standardize <- CausalImpact:::Standardize

  # Test missing input
  checkException(Standardize())

  # Test the basics
  data <- c(-1, 0.1, 1, 2, NA, 3)
  result <- Standardize(data)
  checkTrue(is.list(result))
  checkEquals(names(result), c("y", "UnStandardize"))
  checkEquals(result$UnStandardize(result$y), data)

  # Test the maths
  checkEquals(Standardize(c(1, 2, 3))$y, c(-1, 0, 1))

  # Test that inputs are correctly recovered (including zoo input)
  test.data <- list(c(1), c(1, 1, 1), as.numeric(NA), c(1, NA, 3),
                    zoo(c(10, 20, 30), c(1, 2, 3)))
  lapply(test.data, function(data) {
    result <- Standardize(data)
    checkEquals(result$UnStandardize(result$y), data)
  })

  # Test bad input: matrix
  checkException(Standardize(matrix(rnorm(10), ncol = 2)))
}

# ------------------------------------------------------------------------------
TestStandardizeAllVariables <- function() {
  StandardizeAllVariables <- CausalImpact:::StandardizeAllVariables
  Standardize <- CausalImpact:::Standardize

  # Test empty input
  checkException(StandardizeAllVariables())

  # Test healthy input: several columns
  set.seed(1)
  data <- zoo(cbind(rnorm(100) * 100 + 1000,
                    rnorm(100) * 200 + 2000,
                    rnorm(100) * 300 + 3000))
  result <- StandardizeAllVariables(data)
  checkEquals(length(result), 2)
  checkEquals(names(result), c("data", "UnStandardize"))
  sapply(1 : ncol(result$data), function(c) {
    checkEquals(mean(result$data[, c]), 0, tolerance = 0.0001);
    checkEquals(sd(result$data[, c]), 1, tolerance = 0.0001)
  })
  checkEquals(result$UnStandardize, Standardize(data[, 1])$UnStandardize)

  # Test healthy input: single series only
  set.seed(1)
  data <- zoo(rnorm(100) * 100 + 1000)
  result <- StandardizeAllVariables(data)
  checkEquals(length(result), 2)
  checkEquals(names(result), c("data", "UnStandardize"))
  checkEquals(mean(result$data), 0, tolerance = 0.0001)
  checkEquals(sd(result$data), 1, tolerance = 0.0001)
  checkEquals(result$UnStandardize, Standardize(data)$UnStandardize)
}

# ------------------------------------------------------------------------------
TestInferPeriodIndicesFromData <- function() {
  InferPeriodIndicesFromData <- CausalImpact:::InferPeriodIndicesFromData

  # Test missing input
  checkException(InferPeriodIndicesFromData())

  # Test healthy input
  checkEquals(InferPeriodIndicesFromData(c(10, 20, 30, NA, NA, NA)),
              list(pre.period = c(1, 3), post.period = c(4, 6)))
  checkEquals(InferPeriodIndicesFromData(c(10, NA)),
              list(pre.period = c(1, 1), post.period = c(2, 2)))
  checkEquals(InferPeriodIndicesFromData(c(NA, NA, 10, 20, NA, NA)),
              list(pre.period = c(3, 4), post.period = c(5, 6)))

  # Test bad input
  checkException(InferPeriodIndicesFromData(1))
  checkException(InferPeriodIndicesFromData(NA))
  checkException(InferPeriodIndicesFromData(c(1, 2, 3)))
  checkException(InferPeriodIndicesFromData(c(NA, NA, NA)))
  checkException(InferPeriodIndicesFromData(c(NA, NA, 1, 2, 3)))
}

# ------------------------------------------------------------------------------
TestPrettifyPercentage <- function() {
  PrettifyPercentage <- CausalImpact:::PrettifyPercentage

  checkEquals(PrettifyPercentage(0.05), "+5%")
  checkEquals(PrettifyPercentage(c(0.05, 0.01)), c("+5%", "+1%"))
  checkEquals(PrettifyPercentage(0.05, 1), "+5%")
  checkEquals(PrettifyPercentage(0.1234, 1), "+12.3%")

  # Test documentation example
  checkEquals(PrettifyPercentage(c(-0.125, 0.2), 2), c("-12.5%", "+20%"))
}

# ------------------------------------------------------------------------------
TestPrettifyNumber <- function() {
  PrettifyNumber <- CausalImpact:::PrettifyNumber

  checkEquals(PrettifyNumber(123.456), 123)
  checkEquals(PrettifyNumber(123.456, letter = "K"), "0.1K")
  checkEquals(PrettifyNumber(123.456, letter = "foo"), 123)
  input <- c(0.01, 1, 123, 12345, 1234567, 1982345670)
  output <- c("0", "1", "123", "12.3K", "1.2M", "2B")
  checkEquals(PrettifyNumber(input), output)

  # Test documentation example
  checkEquals(PrettifyNumber(123456), "123.5K")
}

# ------------------------------------------------------------------------------
TestIdentifyNumberAbbreviation <- function() {
  IdentifyNumberAbbreviation <- CausalImpact:::IdentifyNumberAbbreviation

  checkEquals(IdentifyNumberAbbreviation("0.1"), "none")
  checkEquals(IdentifyNumberAbbreviation("0.1K"), "K")
  output <- c("0", "1", "123", "12.3K", "1.2M", "2B")
  letter <- c("none", "none", "none", "K", "M", "B")
  checkEquals(IdentifyNumberAbbreviation(output), letter)

  # Test documentation example
  checkEquals(IdentifyNumberAbbreviation("123.5K"), "K")
}
