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


# Utility functions used throughout the package.
#
# Authors: kbrodersen@google.com (Kay Brodersen)
#          gallusser@google.com (Fabian Gallusser)

# ------------------------------------------------------------------------------
repmat <- function(X, m, n) {
  # R equivalent of repmat (MATLAB). Replicates a given vector or matrix.
  #
  # Args:
  #   X: vector or matrix
  #   m: number of row replications
  #   n: number of column replications
  #
  # Returns:
  #   Matrix
  #
  # Examples:
  #   CausalImpact:::repmat(c(10, 20), 1, 2)
  #   #      [,1] [,2] [,3] [,4]
  #   # [1,]   10   20   10   20

  assert_that(is.vector(X) || is.matrix(X))
  assert_that(is.count(m), is.count(n))
  if (is.vector(X)) {
    X <- t(as.matrix(X))
  }
  mx = dim(X)[1]
  nx = dim(X)[2]
  matrix(t(matrix(X, mx, nx * n)), mx * m, nx * n, byrow = TRUE)
}

# ------------------------------------------------------------------------------
is.wholenumber <- function(x, tol = .Machine$double.eps ^ 0.5) {
  # Checks whether a number is a whole number. This is not the same as
  # \code{is.integer()}, which tests the data type.
  #
  # Args:
  #   x: input scalar or vector
  #   tol: tolerance
  #
  # Returns:
  #   boolean
  #
  # Examples:
  #   CausalImpact:::is.wholenumber(c(1, 1.0, 1.2))
  #   # [1]  TRUE  TRUE FALSE

  return(abs(x - round(x)) < tol)
}

# ------------------------------------------------------------------------------
assert <- function(expr = TRUE, error = "") {
  # Throws a custom error message if a condition is not fulfilled. This is an
  # equally simple and useful R implementation of the corresponding MATLAB
  # function. Note that this function is never silenced, and thus has different
  # semantics than the assert-during-debug-but-silence-in-production idea
  # implemented in other languages.
  #
  # Args:
  #   expr:  expression that evaluates to a logical
  #   error: error message if expression evaluates to \code{FALSE}
  #
  # Returns:
  #   Returns quietly or fails with an error.
  #
  # Examples:
  #   x <- 1
  #   CausalImpact:::assert(x > 0)
  #   CausalImpact:::assert(x > 0, "input argument must be positive")
  #
  # Documentation:
  #   seealso: assert_that

  if (! expr) {
    stop(error, call. = (error == ""))
  }
}

# ------------------------------------------------------------------------------
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

# ------------------------------------------------------------------------------
TryError <- function(call) {
  # Tries to execute <call> and return its return value. If the call fails,
  # returns an error object containing the errror message (but doesn't throw
  # an error and thus doesn't stop execution).
  #
  # Args:
  #   call: any statement, e.g., a variable, a function call, etc.
  #
  # Returns:
  #   The return value of \code{call}. On failure, an error object.

  return(tryCatch(eval(call), error = function(e) e))
}

# ------------------------------------------------------------------------------
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
    assert(length(illegal.args) == 0,
           paste0("illegal extra args: '",
                  paste(illegal.args, collapse = "', '"), "'"))
  }

  # Return
  return(args)
}

# ------------------------------------------------------------------------------
Standardize <- function(y) {
  # Standardizes a vector \code{y} (to obtain mean 0 and SD 1). The original
  # vector can be restored using \code{UnStandardize()}, which is a function
  # that is supplied as part of the return value.
  #
  # Args:
  #   y: numeric vector (may contain \code{NA} values)
  #
  # Returns:
  #   list of:
  #     y: standardized version of the input
  #     UnStandardize: function that restores the original data
  #
  # Examples:
  #   x <- c(1, 2, 3, 4, 5)
  #   result <- CausalImpact:::Standardize(x)
  #   y <- result$UnStandardize(result$y)
  #   stopifnot(isTRUE(all.equal(x, y)))

  assert_that(is.null(dim(y)))
  y.mu <- mean(y, na.rm = TRUE)
  if (is.nan(y.mu)) {
    y.mu <- as.numeric(NA)
  }
  y.sd <- sd(y, na.rm = TRUE)
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

# ------------------------------------------------------------------------------
StandardizeAllVariables <- function(data) {
  # Standardizes all columns of a given time series.
  #
  # Args:
  #   data: data frame or zoo object with one or more columns
  #
  # Returns:
  #   list of:
  #     data: standardized data
  #     UnStandardize: function for undoing the transformation of the first
  #                    column in the provided data

  if (!is.null(ncol(data))) {
    for (j in ncol(data) : 1) {
      tmp <- Standardize(data[, j])
      data[, j] <- tmp$y
      UnStandardize <- tmp$UnStandardize
    }
  } else {
    tmp <- Standardize(data)
    data <- tmp$y
    UnStandardize <- tmp$UnStandardize
  }
  return(list(data = data, UnStandardize = UnStandardize))
}

# ------------------------------------------------------------------------------
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

# ------------------------------------------------------------------------------
PrettifyPercentage <- function(x, round.digits = 0) {
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
  #   # [1] "-12.5%" "+20%"

  sign <- c("-", "+")[(sign(x) + 3) / 2]
  value <- abs(round(100 * x, round.digits))
  return(paste(sign, value, "%", sep = ""))
}

# ------------------------------------------------------------------------------
PrettifyNumber <- function(x, letter = "", round.digits = 1) {
  # Converts a number into heavily rounded human-readible format.
  #
  # Args:
  #   x:            Input scalar or vector of type numeric.
  #   letter:       Thousand value to round to. Possible values: "" (automatic),
  #                 "B" (billion), "M" (million), "K" (thousand), "none" (1).
  #   round.digits: Round the result to this number of decimal places.
  #
  # Returns:
  #   string of formatted values
  #
  # Examples:
  #   CausalImpact:::PrettifyNumber(123456)
  #   # [1] "123.5K"

  letter <- rep(letter, length.out = length(x))
  output <- sapply(1 : length(x), function(index) {
    if ((letter[index] == "" && abs(x[index]) >= 1e9) ||
        letter[index] == "B") {
      paste(round(x[index] / 1e9, round.digits), "B", sep = "")
    } else if ((letter[index] == "" && abs(x[index]) >= 1e6) ||
               letter[index] == "M") {
      paste(round(x[index] / 1e6, round.digits), "M", sep = "")
    } else if ((letter[index] == "" && abs(x[index]) >= 1e3) ||
               letter[index] == "K") {
      paste(round(x[index] / 1e3, round.digits), "K", sep = "")
    } else if (letter[index] == "none") {
      round(x[index], 0)
    } else {
      round(x[index], 0)
    }
  })
  return(output)
}

# ------------------------------------------------------------------------------
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
