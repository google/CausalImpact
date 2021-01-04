\name{CausalImpact}
\alias{CausalImpact}
\title{Inferring causal impact using structural time-series models}
\description{
  \code{CausalImpact()} performs causal inference through
  counterfactual predictions using a Bayesian structural
  time-series model.

  See the package documentation
  (http://google.github.io/CausalImpact/) to understand the
  underlying assumptions. In particular, the model assumes
  that the time series of the treated unit can be explained
  in terms of a set of covariates which were themselves not
  affected by the intervention whose causal effect we are
  interested in.

  The easiest way of running a causal analysis is to call
  \code{CausalImpact()} with \code{data}, \code{pre.period},
  \code{post.period}, \code{model.args} (optional), and
  \code{alpha} (optional). In this case, a time-series model
  is automatically constructed and estimated. The argument
  \code{model.args} offers some control over the model. See
  Example 1 below.

  An alternative is to supply a custom model. In this case,
  the function is called with \code{bsts.model},
  \code{post.period.response}, and \code{alpha} (optional).
  See Example 3 below.
}
\usage{
  CausalImpact(data = NULL, pre.period = NULL,
    post.period = NULL, model.args = NULL,
    bsts.model = NULL, post.period.response = NULL,
    alpha = 0.05)
}
\arguments{
  \item{data}{Time series of response variable and any
  covariates. This can be a \code{zoo} object, a
  \code{vector}, a \code{matrix}, or a \code{data.frame}.
  In any of these cases, the response variable must be in
  the first column, and any covariates in subsequent
  columns. A \code{zoo} object is recommended, as its time
  indices will be used to format the x-axis in
  \code{plot()}.}

  \item{pre.period}{A vector specifying the first and the last time point of the
  pre-intervention period in the response vector \code{y}. This
  period can be thought of as a training period, used to
  determine the relationship between the response variable and
  the covariates. If \code{data} is a \code{zoo} object with
  a \code{time} attribute, \code{pre.period} must be indicated
  using the same time scale (i.e. using the same class as
  \code{time(data)}, see Example 2 below). If \code{data} doesn't have
  a \code{time} attribute, \code{post.period} is indicated with
  indices.}

  \item{post.period}{A vector specifying the first and the last day of the
  post-intervention period we wish to study. This is the period
  after the intervention has begun whose effect we are
  interested in. The relationship between response variable and
  covariates, as determined during the pre-period, will be used
  to predict how the response variable should have evolved
  during the post-period had no intervention taken place. If
  \code{data} is a \code{zoo} object with a \code{time}
  attribute, \code{post.period} must be indicated using the
  same time scale. If \code{data} doesn't have a \code{time}
  attribute, \code{post.period} is indicated with indices.}

  \item{model.args}{Further arguments to adjust the default
  construction of the state-space model used for inference.
  One particularly important parameter is
  \code{prior.level.sd}, which specifies our a priori
  knowledge about the volatility of the data. For even more
  control over the model, you can construct your own model
  using the \code{bsts} package and feed the fitted model
  into \code{CausalImpact()}, as shown in Example 3.}

  \item{bsts.model}{Instead of passing in \code{data} and
  having \code{CausalImpact()} construct a model, it is
  possible to create a custom model using the
  \code{bsts} package. In this case, omit \code{data},
  \code{pre.period}, and \code{post.period}. Instead only
  pass in \code{bsts.model}, \code{post.period.response},
  and \code{alpha} (optional). The model must have been
  fitted on data where the response variable was set to
  \code{NA} during the post-treatment period. The actual
  observed data during this period must then be passed to
  the function in \code{post.period.response}.}

\item{post.period.response}{Actual observed data during
  the post-intervention period. This is required if and
  only if a fitted \code{bsts.model} is provided instead
  of \code{data}.}

  \item{alpha}{Desired tail-area probability for posterior
  intervals. Defaults to 0.05, which will produce central
  95\% intervals.}
}
\value{
  \code{CausalImpact()} returns a \code{CausalImpact}
  object containing the original observed response, its
  counterfactual predictions, as well as pointwise and
  cumulative impact estimates along with posterior credible
  intervals. Results can summarised using \code{summary()}
  and visualized using \code{plot()}. The object is a list
  with the following fields:

  \itemize{
    \item \code{series}. Time-series object (\code{zoo})
    containing the original response \code{response} as
    well as the computed inferences. The added columns are
    listed in the table below.
    \item \code{summary}. Summary statistics for the
    post-intervention period. This includes the posterior
    expectation of the overall effect, the corresponding
    posterior credible interval, and the posterior
    probability that the intervention had any effect,
    expressed in terms of a one-sided p-value. Note that
    checking whether the posterior interval includes zero
    corresponds to a two-sided hypothesis test. In contrast,
    checking whether the p-value is below \code{alpha}
    corresponds to a one-sided hypothesis test.
    \item \code{report}. A suggested verbal
    interpretation of the results.
    \item \code{model}. A list with four elements \code{pre.period},
    \code{post.period}, \code{bsts.model} and \code{alpha}. \code{pre.period}
    and \code{post.period} indicate the first and last time point of
    the time series in the respective period, \code{bsts.model} is
    the fitted model returned by \code{bsts()}, and \code{alpha}
    is the user-specified tail-area probability.
  }

  The field \code{series} is a
    \code{zoo} time-series object with the following columns:

    \tabular{ll}{
      \code{response} \tab
      Observed response as supplied to \code{CausalImpact()}. \cr
      \code{cum.response} \tab
      Cumulative response during the modeling period. \cr
      \code{point.pred} \tab
      Posterior mean of counterfactual predictions. \cr
      \code{point.pred.lower} \tab
      Lower limit of a (\code{1 - alpha}) posterior interval. \cr
      \code{point.pred.upper} \tab
      Upper limit of a (\code{1 - alpha}) posterior interval. \cr
      \code{cum.pred} \tab
      Posterior cumulative counterfactual predictions. \cr
      \code{cum.pred.lower} \tab
      Lower limit of a (\code{1 - alpha}) posterior interval.
      \cr
      \code{cum.pred.upper} \tab
      Upper limit of a (\code{1 - alpha}) posterior interval.
      \cr
      \code{point.effect} \tab
      Point-wise posterior causal effect. \cr
      \code{point.effect.lower} \tab
      Lower limit of the posterior interval (as above). \cr
      \code{point.effect.lower} \tab Upper limit of the
      posterior interval (as above). \cr
      \code{cum.effect} \tab
      Posterior cumulative effect. \cr
      \code{cum.effect.lower} \tab
      Lower limit of the posterior interval (as above). \cr
      \code{cum.effect.upper} \tab
      Upper limit of the posterior interval (as above). \cr
    }
}
\note{
  Optional arguments can be passed as a list in \code{model.args},
  providing additional control over model construction:

  \itemize{
    \item \code{niter}. Number of MCMC samples to draw. Higher numbers
    yield more accurate inferences. Defaults to 1000.

    \item \code{standardize.data}. Whether to standardize all columns of the
    data using moments estimated from the pre-intervention period before fitting
    the model. This is equivalent to an empirical Bayes approach to setting the
    priors. It ensures that results are invariant to linear transformations of
    the data. Defaults to \code{TRUE}.

    \item \code{prior.level.sd}. Prior standard deviation of the Gaussian random
    walk of the local level, expressed in terms of data standard deviations.
    Defaults to 0.01, a typical choice for well-behaved and stable datasets
    with low residual volatility after regressing out known predictors (e.g.,
    web searches or sales in high quantities). When in doubt, a safer option is
    to use 0.1, as validated on synthetic data, although this may sometimes give
    rise to unrealistically wide prediction intervals.

    \item \code{nseasons}. Period of the seasonal components. In order to
    include a seasonal component, set this to a whole number greater than 1. For
    example, if the data represent daily observations, use 7 for a day-of-week
    component. This interface currently only supports up to one seasonal
    component. To specify multiple seasonal components, use \code{bsts} to
    specify the model directly, then pass the fitted model in as
    \code{bsts.model}. Defaults to 1, which means no seasonal component is used.

    \item \code{season.duration}. Duration of each season, i.e., number of data
    points each season spans. For example, to add a day-of-week component to
    data with daily granularity, supply the arguments
    \code{model.args = list(nseasons = 7, season.duration = 1)}.
    Alternatively, use
    \code{model.args = list(nseasons = 7, season.duration = 24)}
    to add a day-of-week component to data with hourly granularity.
    Defaults to 1.

    \item \code{dynamic.regression}. Whether to include time-varying regression
    coefficients. In combination with a time-varying local trend or even a
    time-varying local level, this often leads to overspecification, in which
    case a static regression is safer. Defaults to \code{FALSE}.

    \item \code{max.flips}. By default, each iteration tries to flip each
    predictor in or out of the model.  Setting \code{max.flips} tells the
    algorithm to randomly choose that many variables to explore flipping. Useful
    to set (e.g. 100) when you have a large number of controls (e.g. 10000); in
    such cases, a lower \code{max.flips} can speed up computation. Defaults to
    -1.
  }
}
\author{
  Kay H. Brodersen \email{kbrodersen@google.com}
}
\examples{
# Example 1
#
# Example analysis on a simple artificial dataset
# consisting of a response variable y and a
# single covariate x1.
set.seed(1)
x1 <- 100 + arima.sim(model = list(ar = 0.999), n = 52)
y <- 1.2 * x1 + rnorm(52)
y[41:52] <- y[41:52] + 10
data <- cbind(y, x1)
pre.period <- c(1, 40)
post.period <- c(41, 52)
impact <- CausalImpact(data, pre.period, post.period)

# Print and plot results
summary(impact)
summary(impact, "report")
plot(impact)
plot(impact, "original")
plot(impact$model$bsts.model, "coefficients")

# For further output, type:
names(impact)

\dontrun{
# Example 2
#
# Weekly time series: same data as in example 1, annotated
# with dates.
times <- seq.Date(as.Date("2016-01-03"), by = 7, length.out = 52)
data <- zoo(cbind(y, x1), times)

impact <- CausalImpact(data, times[pre.period], times[post.period])

summary(impact)  # Same as in example 1.
plot(impact)  # The plot now shows dates on the x-axis.

# Example 3
#
# For full flexibility, specify a custom model and pass the
# fitted model to CausalImpact(). To run this example, run
# the code for Example 1 first.
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddLocalLevel(list(), y)
bsts.model <- bsts(y ~ x1, ss, niter = 1000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
plot(impact)
}
}
