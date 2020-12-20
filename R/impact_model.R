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

# Bayesian dynamic diffusion-regression state-space model for computing
# counterfactual predictions in a time series. Uses an MCMC algorithm
# implemented in the \code{bsts} package to compute samples from the posterior
# (smoothing) densities over states and parameters.
#
# Author: kbrodersen@google.com (Kay Brodersen)

# Some model priors are fixed, others can be adjusted through model.args.
# For full flexibility, construct your own bsts model and feed it into
# CausalImpactForBsts().
kLocalLevelPriorSampleSize <- 32
kStaticRegressionExpectedModelSize <- 3
kStaticRegressionExpectedR2 <- 0.8
kStaticRegressionPriorDf <- 50
kDynamicRegressionPriorSampleSize <- 32

ObservationsAreIllConditioned <- function(y) {
  # Checks whether the response variable (i.e., the series of observations for
  # the dependent variable y) are ill-conditioned. For example, the series might
  # contain too few non-NA values. In such cases, inference will be aborted.
  #
  # Args:
  #   y: observed series (numeric vector or single zoo series)
  #
  # Returns:
  #   TRUE if something is wrong with the observations; FALSE otherwise.

  assert_that(!is.null(y), length(y) >= 1)
  ill.conditioned <- FALSE

  # All NA?
  if (all(is.na(y))) {
    warning("Aborting inference due to input series being all NA.")
    ill.conditioned <- TRUE

  # Fewer than 3 non-NA values?
  } else if (sum(!is.na(y)) < 3) {
    warning("Aborting inference due to fewer than 3 non-NA values in input")
    ill.conditioned <- TRUE

  # Constant series?
  } else if (sd(y, na.rm = TRUE) == 0) {
    warning(paste0("Aborting inference due to input series being constant: ",
                   y[!is.na(y)][1]))
    ill.conditioned <- TRUE
  }
  return(ill.conditioned)
}

FormatInputForConstructModel <- function(data, model.args) {
  # Checks the input arguments supplied to ConstructModel(). Missing arguments
  # in \code{model.args} will be filled using \code{.defaults} (see top of file
  # impact_analysis.R).
  #
  # Args:
  #   data: time series of response variable and covariates
  #   model.args: list of additional arguments
  #
  # Returns:
  #   list of checked and correctly formatted arguments

  # Check <data>
  assert_that(!is.null(data))
  data <- as.zoo(data)
  if (is.null(ncol(data))) {
    dim(data) <- c(length(data), 1)
  }
  assert_that(is.numeric(data))
  assert_that(nrow(data) > 0)

  # If <data> has no names, assign: y, x1, x2, ...
  if (is.null(names(data))) {
    if (ncol(data) == 1) {
      names(data)[1] <- "y"
    } else {
      names(data) <- c("y", paste0("x", 2:ncol(data) - 1))
    }
  }

  # Check covariates
  if (ncol(data) >= 2) {
    assert_that(all(!is.na(data[, -1])), msg = "covariates must not be NA")
  }

  # (Re-)parse <model.args>, fill gaps using <.defaults>
  # (defined in impact_analysis.R)
  model.args <- ParseArguments(model.args, .defaults)

  # Check those parts of <model.args> that are used in this file
  # Check <niter>
  assert_that(is.scalar(model.args$niter))
  assert_that(is.numeric(model.args$niter))
  assert_that(!is.na(model.args$niter))
  assert_that(is.wholenumber(model.args$niter))
  model.args$niter <- round(model.args$niter)
  assert_that(model.args$niter >= 10,
              msg = paste0("must draw, at the very least, 10 MCMC samples; ",
                           "recommending 1000"))
  if (model.args$niter < 1000) {
    warning("Results potentially inaccurate. Consider using more MCMC samples.")
  }

  # Check <prior.level.sd>
  assert_that(is.scalar(model.args$prior.level.sd))
  assert_that(is.numeric(model.args$prior.level.sd))
  assert_that(!is.na(model.args$prior.level.sd))
  assert_that(model.args$prior.level.sd > 0)

  # Check <nseasons>
  assert_that(is.scalar(model.args$nseasons))
  assert_that(is.numeric(model.args$nseasons))
  assert_that(!is.na(model.args$nseasons))
  assert_that(is.wholenumber(model.args$nseasons))
  assert_that(model.args$nseasons >= 1,
              msg = paste0("nseasons cannot be 0; use 1 in order not to have ",
                           "seaonsal components"))

  # Check <season.duration>
  assert_that(is.scalar(model.args$season.duration))
  assert_that(is.numeric(model.args$season.duration))
  assert_that(!is.na(model.args$season.duration))
  assert_that(is.wholenumber(model.args$season.duration))
  assert_that(model.args$season.duration >= 1)

  # Check <dynamic.regression>
  assert_that(is.scalar(model.args$dynamic.regression))
  assert_that(is.logical(model.args$dynamic.regression))
  assert_that(!is.na(model.args$dynamic.regression))

  # Check <max.flips>
  assert_that(is.scalar(model.args$max.flips))
  assert_that(is.numeric(model.args$max.flips))
  assert_that(!is.na(model.args$max.flips))
  assert_that(is.wholenumber(model.args$max.flips))
  assert_that(model.args$max.flips > 0 || model.args$max.flips == -1)

  # Return updated args
  return(list(data = data, model.args = model.args))
}

# Tell 'R CMD check' to treat `BstsOptions()` as global variable to avoid
# false positives as long as 'bsts' version 0.7.x is not published.
# TODO(alhauser): remove this when 'bsts' version 0.7.x is published.
if(getRversion() >= "2.15.1") {
  utils::globalVariables("BstsOptions")
}

ConstructModel <- function(data, model.args = NULL) {
  # Specifies the model and performs inference. Inference means using the data
  # to pass from a prior distribution over parameters and states to a posterior
  # distribution. In a Bayesian framework, estimating a model means to obtain
  # p(parameters | data) from p(data | parameters) and p(parameters). This
  # involves multiplying the prior with the likelihood and normalising the
  # resulting distribution using the marginal likelihood or model evidence,
  # p(data). Computing the evidence poses a virtually intractable
  # high-dimensional integration problem which can be turned into an easier
  # optimization problem using, for instance, an approximate stochastic
  # inference strategy. Here, we use a Markov chain Monte Carlo algorithm, as
  # implemented in the \code{bsts} package.
  #
  # Args:
  #   data: time series of response variable and optional covariates
  #   model.args: optional list of additional model arguments
  #
  # Returns:
  #   \code{bsts.model}, as returned by \code{bsts()}

  # Check and format input
  checked <- FormatInputForConstructModel(data, model.args)
  data <- checked$data
  model.args <- checked$model.args
  y <- data[, 1]

  # If the series is ill-conditioned, abort inference and return NULL
  if (ObservationsAreIllConditioned(y)) {
    return(NULL)
  }

  # Local level
  # sigma.guess: standard deviation of the random walk of the level
  sdy <- sd(y, na.rm = TRUE)
  ss <- list()
  sd.prior <- SdPrior(sigma.guess = model.args$prior.level.sd * sdy,
                      upper.limit = sdy,
                      sample.size = kLocalLevelPriorSampleSize)
  ss <- AddLocalLevel(ss, y, sigma.prior = sd.prior)

  # Add seasonal component?
  if (model.args$nseasons > 1) {
    ss <- AddSeasonal(ss, y,
                      nseasons = model.args$nseasons,
                      season.duration = model.args$season.duration)
  }

  # No regression?
  if (ncol(data) == 1) {
    bsts.model <- bsts(y, state.specification = ss, niter = model.args$niter,
                       seed = 1, ping = 0,
                       model.options =
                           BstsOptions(save.prediction.errors = TRUE),
                       max.flips = model.args$max.flips)
  } else {
    formula <- paste0(names(data)[1], " ~ .")

    # Static regression?
    if (!model.args$dynamic.regression) {
      bsts.model <- bsts(formula, data = data, state.specification = ss,
                         expected.model.size =
                             kStaticRegressionExpectedModelSize,
                         expected.r2 = kStaticRegressionExpectedR2,
                         prior.df = kStaticRegressionPriorDf,
                         niter = model.args$niter, seed = 1, ping = 0,
                         model.options =
                             BstsOptions(save.prediction.errors = TRUE),
                         max.flips = model.args$max.flips)
      time(bsts.model$original.series) <- time(data)

    # Dynamic regression?
    } else {
      # Since we have predictor variables in the model, we need to explicitly
      # make their coefficients time-varying using AddDynamicRegression(). In
      # bsts(), we are therefore not giving a formula but just the response
      # variable. We are then using SdPrior to only specify the prior on the
      # residual standard deviation.
      # prior.mean: precision of random walk of coefficients
      sdx <- apply(data[, -1, drop = FALSE], 2, function(x) sd(x, na.rm = TRUE))
      model.options <- DynamicRegressionRandomWalkOptions(sdx = sdx, sdy = sdy)
      ss <- AddDynamicRegression(ss, formula, data = data,
                                 model.options = model.options)
      sd.prior <- SdPrior(sigma.guess = model.args$prior.level.sd * sdy,
                          upper.limit = 0.1 * sdy,
                          sample.size = kDynamicRegressionPriorSampleSize)
      bsts.model <- bsts(y, state.specification = ss, niter = model.args$niter,
                         expected.model.size = 3, ping = 0, seed = 1,
                         prior = sd.prior, max.flips = model.args$max.flips)
    }
  }
  return(bsts.model)
}
