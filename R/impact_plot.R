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

# Code for plotting the result of a CausalImpact analysis.
#
# Author: kbrodersen@google.com (Kay Brodersen)

CreateDataFrameForPlot <- function(impact) {
  # Creates a long-format data frame for CreateImpactPlot().
  #
  # Args:
  #   impact: \code{CausalImpact} results object
  #
  # Returns:
  #   data frame of: time, response, mean, lower, upper, metric

  # Check input
  assert_that((class(impact) == "CausalImpact"))
  assert(!isTRUE(all(is.na(impact$series[, -c(1, 2)]))),
         "inference was aborted; cannot create plot")

  # Create data frame from zoo series
  data <- as.data.frame(impact$series)
  data <- cbind(time = time(impact$series), data)

  # Reshape data frame
  tmp1 <- data[, c("time", "response", "point.pred", "point.pred.lower",
                   "point.pred.upper")]
  names(tmp1) <- c("time", "response", "mean", "lower", "upper")
  tmp1$baseline <- NA
  tmp1$metric <- "original"
  tmp2 <- data[, c("time", "response", "point.effect", "point.effect.lower",
                   "point.effect.upper")]
  names(tmp2) <- c("time", "response", "mean", "lower", "upper")
  tmp2$baseline <- 0
  tmp2$metric <- "pointwise"
  tmp2$response <- NA
  tmp3 <- data[, c("time", "response", "cum.effect", "cum.effect.lower",
                   "cum.effect.upper")]
  names(tmp3) <- c("time", "response", "mean", "lower", "upper")
  tmp3$metric <- "cumulative"
  tmp3$baseline <- 0
  tmp3$response <- NA
  data <- rbind(tmp1, tmp2, tmp3)
  data$metric <- factor(data$metric, c("original", "pointwise", "cumulative"))
  rownames(data) <- NULL
  return(data)
}

CreatePeriodMarkers <- function(pre.period, post.period, times) {
  # Creates a vector of period markers to display.
  #
  # Args:
  #   pre.period:  vector of 2 time points that define the pre-period.
  #   post.period: vector of 2 time points that define the post-period.
  #   times:       vector of time points.
  #
  # Returns:
  #   Vector of period markers that should be displayed, generally depicting the
  #   first and last time points of pre- and post-period. The start of the pre-
  #   period is not shown if it coincides with the first time point of the time
  #   series; similarly, the last time point of the post-period is not shown if
  #   it coincides with the last time point of the series. If there is no gap
  #   between pre- and post-period, the start marker of the post-period is
  #   omitted.

  pre.period.indices <- GetPeriodIndices(pre.period, times)
  post.period.indices <- GetPeriodIndices(post.period, times)
  markers <- NULL
  if (pre.period.indices[1] > 1) {
    markers <- c(markers, times[pre.period.indices[1]])
  }
  markers <- c(markers, times[pre.period.indices[2]])
  if (pre.period.indices[2] < post.period.indices[1] - 1) {
    markers <- c(markers, times[post.period.indices[1]])
  }
  if (post.period.indices[2] < length(times)) {
    markers <- c(markers, times[post.period.indices[2]])
  }
  markers <- as.numeric(markers)
  return(markers)
}

# Tell R CMD check to treat columns of data frames used in `ggplot` functions
# as global variables; this avoids false positives of "no visible binding for
# global variable ..." during the check.
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("baseline", "lower", "response", "upper"))
}

CreateImpactPlot <- function(impact, metrics = c("original", "pointwise",
                                                 "cumulative")) {
  # Creates a plot of observed data and counterfactual predictions.
  #
  # Args:
  #   impact:  \code{CausalImpact} results object returned by
  #            \code{CausalImpact()}.
  #   metrics: Which metrics to include in the plot. Can be any combination of
  #            "original", "pointwise", and "cumulative".
  #
  # Returns:
  #   A ggplot2 object that can be plotted using plot().

  # Create data frame of: time, response, mean, lower, upper, metric
  data <- CreateDataFrameForPlot(impact)

  # Select metrics to display (and their order)
  assert_that(is.vector(metrics))
  metrics <- match.arg(metrics, several.ok = TRUE)
  data <- data[data$metric %in% metrics, ]
  data$metric <- factor(data$metric, metrics)

  # Initialize plot
  q <- ggplot(data, aes(x = time)) + theme_bw(base_size = 15)
  q <- q + xlab("") + ylab("")
  if (length(metrics) > 1) {
    q <- q + facet_grid(metric ~ ., scales = "free_y")
  }

  # Add prediction intervals
  q <- q + geom_ribbon(aes(ymin = lower, ymax = upper),
                       data, fill = "SlateGray2")

  # Add pre-period markers
  xintercept <- CreatePeriodMarkers(impact$model$pre.period,
                                    impact$model$post.period,
                                    time(impact$series))
  q <- q + geom_vline(xintercept = xintercept,
                      colour = "darkgrey", size = 0.8, linetype = "dashed")

  # Add zero line to pointwise and cumulative plot
  q <- q + geom_line(aes(y = baseline),
                     colour = "darkgrey", size = 0.8, linetype = "solid")

  # Add point predictions
  q <- q + geom_line(aes(y = mean), data,
                     size = 0.6, colour = "darkblue", linetype = "dashed")

  # Add observed data
  q <- q + geom_line(aes(y = response), size = 0.6)
  return(q)
}

plot.CausalImpact <- function(x, ...) {
  # Creates a plot of observed data and counterfactual predictions.
  #
  # Args:
  #   x:   A \code{CausalImpact} results object, as returned by
  #        \code{CausalImpact()}.
  #   ...: Can be used to specify \code{metrics}, which determines which panels
  #        to include in the plot. The argument \code{metrics} can be any
  #        combination of "original", "pointwise", "cumulative". Partial matches
  #        are allowed.
  #
  # Returns:
  #   A ggplot2 object that can be plotted using plot().
  #
  # Examples:
  #   \dontrun{
  #   impact <- CausalImpact(...)
  #
  #   # Default plot:
  #   plot(impact)
  #
  #   # Customized plot:
  #   impact.plot <- plot(impact) + ylab("Sales")
  #   plot(impact.plot)
  #   }

  return(CreateImpactPlot(x, ...))
}
