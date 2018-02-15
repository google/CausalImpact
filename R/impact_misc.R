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

# Utility functions used throughout the package.
#
# Authors: kbrodersen@google.com (Kay Brodersen)
#          gallusser@google.com (Fabian Gallusser)
#          alhauser@google.com (Alain Hauser)

is.wholenumber <- function(x, tolerance = .Machine$double.eps ^ 0.5) {
  # Checks whether a number is a whole number. This is not the same as
  # \code{is.integer()}, which tests the data type.
  #
  # Args:
  #   x:         input scalar or vector
  #   tolerance: tolerance
  #
  # Returns:
  #   boolean
  #
  # Examples:
  #   CausalImpact:::is.wholenumber(c(1, 1.0, 1.2))
  #   # [1]  TRUE  TRUE FALSE

  return(abs(x - round(x)) < tolerance)
}

cumsum.na.rm <- function(x) {
  # Cumulative sum, ignoring NA.
  #
  # Args:
  #   x: numeric vector
  #
  # Returns:
  #   cumulative sum of x, but NA values are ignored
  #
  # Examples:
  #   CausalImpact:::cumsum.na.rm(c(1, NA, 2))
  #   # [1]  1 NA 3
  #
  #   # Compare this to the conventional cumsum():
  #   cumsum(c(1, NA, 2))
  #   # [1]  1 NA NA

  if (is.null(x)) {
    return(x)
  }
  nas <- is.na(x)
  s <- cumsum(ifelse(nas, 0, x))
  s[nas] <- NA
  return(s)
}

is.numerically.equal <- function(x, y, tolerance = .Machine$double.eps ^ 0.5) {
  # Tests whether two numbers are 'numerically equal' by checking whether their
  # relative difference is smaller than a given tolerance. 'Relative difference'
  # is defined as the absolute difference of the values divided by the maximum
  # of their absolute values.
  #
  # The difference between this function and `all.equal` is that the latter
  # checks the absolute difference rather than the relative difference.
  #
  # Args:
  #   x:         one of the values to be compared.
  #   y:         one of the values to be compared.
  #   tolerance: tolerance for the relative difference of `x` and `y`.

  assert_that(is.numeric(x), is.scalar(x))
  assert_that(is.numeric(y), is.scalar(y))
  assert_that(is.numeric(tolerance), is.scalar(tolerance), tolerance > 0)

  if (x == 0 && y == 0) {
    return(TRUE)
  } else {
    relative.difference <- abs(x - y) / max(abs(x), abs(y))
    return(relative.difference <= tolerance)
  }
}

TryStop <- function(call, error.msg = NULL) {
  # Tries to evaluate \code{call} and return its return value. If the call
  # fails, throws an error. The error message can be overwritten by
  # \code{error.msg}.
  #
  # Args:
  #   call:      any statement, e.g., a variable, a function call, etc.
  #   error.msg: message to show if \code{call} fails
  #
  # Returns:
  #   The return value of \code{call}. On failure, throws an error.
  #
  # Examples:
  #   \dontrun{
  #   ts <- CausalImpact:::TryStop(as.zoo(data), "failed to convert input data")
  #   }

  if (is.null(error.msg)) {
    return(eval(call))
  } else {
    return(tryCatch(eval(call), error = function(e) stop(error.msg)))
  }
}

ParseArguments <- function(args, defaults, allow.extra.args = FALSE) {
  # Fills missing fields in \code{args} with \code{defaults}. This function is
  # similar to what \code{modifyList()} does; except it is not nested, it allows
  # extra flexibility for errors, and \code{NULL} values in \code{args} do not
  # override the defaults.
  #
  # Args:
  #   args:             A list of arguments of any type.
  #
  #   defaults:         A list of default values.
  #
  #   allow.extra.args: Whether to allow (and keep) additional arguments in
  #                     \code{args} that are not present in \code{defaults}.
  #
  # Returns:
  #   \code{defaults}, where any value that is present in \code{args} has been
  #   overridden.
  #
  # Examples:
  #   args <- list(a = 10)
  #   defaults <- list(a = 1, b = 2)
  #   args <- CausalImpact:::ParseArguments(args, defaults)
  #   # Result: a = 10, b = 2

  # Check input
  assert_that(!is.null(defaults))
  assert_that(is.list(defaults))
  assert_that(is.list(args) || is.null(args))

  # Merge
  if (is.null(args)) {
    args <- list()
  }
  for (arg in names(defaults)) {
    if (!(arg %in% names(args)) || (is.null(args[[arg]]))) {
      args[[arg]] <- defaults[[arg]]
    }
  }

  # Are extra args allowed?
  if (!allow.extra.args) {
    illegal.args <- setdiff(names(args), names(defaults))
    assert_that(length(illegal.args) == 0,
                msg = paste0("illegal extra args: '",
                             paste(illegal.args, collapse = "', '"), "'"))
  }

  # Return
  return(args)
}

Standardize <- function(y, fit.range = NULL) {
  # Standardizes a vector \code{y}. The resulting vector is a linear
  # transformation of the entire vector \code{y} which has mean 0 and standard
  # deviation 1 over the range of indices specified by \code{fit.range}; i.e.
  # the function transforms the entire vector, but uses only part of it to fit
  # the moments. The original vector can be restored using
  # \code{UnStandardize()}, which is a function that is supplied as part of the
  # return value.
  #
  # Args:
  #   y:         numeric vector (may contain \code{NA} values) to be
  #              standardized
  #   fit.range: vector with 2 entries specifying the first and last index
  #              of the range of \code{y} used to fit the moments. If
  #              \code{NULL}, the whole range of \code{y} is used.
  #
  # Returns:
  #   list of:
  #     y:             standardized input vector, i.e. linearly transformed
  #                    input having mean 0 and SD 1 over the range of indices
  #                    specified by \code{fit.range}.
  #     UnStandardize: function that restores the original data.
  #
  # Examples:
  #   x <- c(1, 2, 3, 4, 5)
  #   result <- CausalImpact:::Standardize(x, c(1, 3))
  #   y <- result$UnStandardize(result$y)
  #   stopifnot(isTRUE(all.equal(x, y)))

  assert_that(is.null(dim(y)))
  if (!is.null(fit.range)) {
    assert_that(is.numeric(fit.range), length(fit.range) == 2,
                !anyNA(fit.range), !is.unsorted(c(1, fit.range, length(y))))
  } else {
    fit.range <- c(1, length(y))
  }

  y.fit <- y[fit.range[1] : fit.range[2]]
  y.mu <- mean(y.fit, na.rm = TRUE)
  if (is.nan(y.mu)) {
    y.mu <- NA_real_
  }
  y.sd <- sd(y.fit, na.rm = TRUE)
  y <- y - y.mu
  if (!is.na(y.sd) && (y.sd > 0)) {
    y <- y / y.sd
  }
  UnStandardize <- function(y) {
    if (!is.na(y.sd) && (y.sd > 0)) {
      y <- y * y.sd
    }
    y <- y + y.mu
    return(y)
  }
  return(list(y = y, UnStandardize = UnStandardize))
}

StandardizeAllVariables <- function(data, fit.range = NULL) {
  # Standardizes all columns of a given time series. While it transforms entire
  # columns, it just uses the rows specified by \code{fit.range} to fit the
  # moments (mean and standard deviation).
  #
  # Args:
  #   data:      data frame or zoo object with one or more columns
  #   fit.range: vector with 2 entries specifying the first and last row
  #              of the range of \code{data} used to fit the moments. If
  #              \code{NULL}, all rows of \code{data} are used.
  #
  # Returns:
  #   list of:
  #     data: standardized data
  #     UnStandardize: function for undoing the transformation of the first
  #                    column in the provided data

  if (!is.null(ncol(data))) {
    for (j in ncol(data) : 1) {
      tmp <- Standardize(data[, j], fit.range)
      data[, j] <- tmp$y
      UnStandardize <- tmp$UnStandardize
    }
  } else {
    tmp <- Standardize(data, fit.range)
    data <- tmp$y
    UnStandardize <- tmp$UnStandardize
  }
  return(list(data = data, UnStandardize = UnStandardize))
}

GetPeriodIndices <- function(period, times) {
  # Computes indices belonging to a period in data.
  #
  # Args:
  #   period:  two-element vector specifying start and end of a period, having
  #            the same data type as `times. The range from `period[1]` to
  #            `period[2]` must have an intersect with `times`.
  #   times:   vector of time points; can be of integer or of POSIXct type.
  #
  # Returns:
  #   A two-element vector with the indices of the period start and end within
  #   `times`.

  # Check input
  assert_that(length(period) == 2)
  assert_that(!anyNA(times))
  assert_that(identical(class(period), class(times)) ||
              (is.numeric(period) && is.numeric(times)))
  # Check if period boundaries are in the right order, and if `period` has an
  # overlap with `times`.
  assert_that(period[1] <= period[2])
  assert_that(period[1] <= tail(times, 1), period[2] >= times[1])

  # Look up values of start and end of period in `times`; also works if the
  # period start and end time are not exactly present in the time series.
  indices <- seq_along(times)
  is.period <- (period[1] <= times) & (times <= period[2])
  # Make sure the period does match any time points.
  assert_that(any(is.period),
              msg = "The period must cover at least one data point")
  period.indices <- range(indices[is.period])
  return(period.indices)
}

InferPeriodIndicesFromData <- function(y) {
  # Takes in a vector of observations and guesses the beginning and end of the
  # pre-period and the post-period.
  #
  # Args:
  #   y: observation vector
  #
  # Returns:
  #   pre.period: beginning and end of pre-period
  #   post.period: beginning and end of post-period
  #
  # Examples:
  #   CausalImpact:::InferPeriodIndicesFromData(c(10, 20, 30, 40, NA, NA, NA))
  #   # $pre.period
  #   # [1] 1 4
  #   #
  #   # $post.period
  #   # [1] 5 7

  assert_that(is.numeric(y))
  assert_that(length(y) >= 2)
  assert_that(is.na(tail(y, 1)))
  pre.period <- rep(NA, 2)
  tmp <- which(!is.na(y))[1]
  assert_that(length(tmp) != 0)
  pre.period[1] <- tmp
  tmp <- tail(which(diff(is.na(y)) == 1), 1)
  assert_that(length(tmp) != 0)
  pre.period[2] <- tmp
  post.period <- rep(NA, 2)
  post.period[1] <- pre.period[2] + 1
  post.period[2] <- length(y)
  return(list(pre.period = pre.period, post.period = post.period))
}

PrettifyPercentage <- function(x, round.digits = 0L) {
  # Converts a number into a nicely formatted percentage.
  #
  # Args:
  #  x:            Input scalar or vector of type numeric.
  #  round.digits: Round resulting percentage to this number of decimal places.
  #
  # Returns:
  #   vector of characters (same length as <vector>) of percentages
  #
  # Examples:
  #   CausalImpact:::PrettifyPercentage(c(-0.125, 0.2), 2)
  #   # [1] "-12.50%" "+20.00%"

  # Check input
  assert_that(all(is.finite(x)))
  assert_that(is.numeric(round.digits), is.scalar(round.digits),
              round.digits >= 0)
  round.digits <- as.integer(round.digits)

  return(sprintf("%+0.*f%%", round.digits, 100 * x))
}

PrettifyNumber <- function(x, letter = "", round.digits = 1L) {
  # Converts a number into heavily rounded human-readible format.
  #
  # Args:
  #   x:            Input scalar or vector of type numeric.
  #   letter:       Thousand value to round to. Possible values: "" (automatic),
  #                 "B" (billion), "M" (million), "K" (thousand), "none" (1).
  #   round.digits: Round the result to this number of decimal places if abs(x)
  #                 is at least 1 or <letter> is specified. If abs(x) is less
  #                 than 1, and if no <letter> is specified, <round.digits> is
  #                 interpreted as the number of significant digits.
  #
  # Returns:
  #   string of formatted values
  #
  # Examples:
  #   CausalImpact:::PrettifyNumber(c(0.123, 123, 123456))
  #   # [1] "0.1"    "123.0"  "123.5K"
  #   CausalImpact:::PrettifyNumber(3995, letter = "K", round.digits = 2)
  #   # [1] "4.00K"
  #   CausalImpact:::PrettifyNumber(1.234e-3, round.digits = 2)
  #   # [1] "0.0012"

  # Check input
  assert_that(is.numeric(x))
  assert_that(is.character(letter))
  assert_that(all(letter %in% c("", "B", "M", "K", "none")))
  assert_that(is.numeric(round.digits), round.digits[1] >= 0)
  round.digits <- as.integer(round.digits[1])

  letter <- rep(letter, length.out = length(x))
  PrettifySingleNumber <- function(x, letter, round.digits) {
    if (is.na(x) && !is.nan(x)) {
      return("NA")
    } else if (!is.finite(x)) {
      return(as.character(x))
    } else if ((letter == "" && abs(x) >= 1e9) || letter == "B") {
      return(sprintf("%0.*fB", round.digits, x / 1e9))
    } else if ((letter == "" && abs(x) >= 1e6) || letter == "M") {
      return(sprintf("%0.*fM", round.digits, x / 1e6))
    } else if ((letter == "" && abs(x) >= 1e3) || letter == "K") {
      return(sprintf("%0.*fK", round.digits, x / 1e3))
    } else if (abs(x) >= 1 || x == 0) {
      return(sprintf("%0.*f", round.digits, x))
    } else {
      # Calculate position of first non-zero digit after the decimal point
      first.nonzero <- - floor(log10(abs(x)))
      return(sprintf("%0.*f", round.digits + first.nonzero - 1, x))
    }
  }
  output <- sapply(seq_along(x), function(index) {
    PrettifySingleNumber(x[index], letter[index], round.digits)})
  return(output)
}

IdentifyNumberAbbreviation <- function(abbreviated.number) {
  # Identifies the rounding thousand used in PrettifyNumber(). It is useful when
  # multiple numbers to be rounded at the same level.
  #
  # Args:
  #   abbreviated.number: Abbreviated number, as for example returned by
  #                       PrettifyNumber().
  #
  # Returns:
  #   "B", "M", "K", or "none"
  #
  # Examples:
  #   CausalImpact:::IdentifyNumberAbbreviation("123.5K")
  #   # [1] "K"

  letter <- substr(abbreviated.number, nchar(abbreviated.number),
                   nchar(abbreviated.number))
  letter[!is.element(letter, c("B", "M", "K"))] <- "none"
  return(letter)
}
