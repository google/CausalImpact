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

# Unit tests for impact_plot.R.
#
# Author: kbrodersen@google.com (Kay Brodersen)

TestCreateImpactPlot <- function() {
  CreateImpactPlot <- CausalImpact:::CreateImpactPlot

  # Test empty input
  checkException(CreateImpactPlot())

  # Test input with integer time indices
  x <- 1 : 20
  y <- x + rnorm(20) + c(rep(0, 10), rep(10, 10))
  data <- zoo(cbind(y, x))
  pre.period <- c(1, 10)
  post.period <- c(11, 20)
  model.args <- list(niter = 500)
  impact <- CausalImpact(data, pre.period, post.period, model.args)
  q <- CreateImpactPlot(impact)
  checkEquals(class(q), c("gg", "ggplot"))
  plot(q)

  # Test input with Date time indices
  data <- zoo(cbind(y, x),
              seq.Date(as.Date("2014-01-01"), as.Date("2014-01-20"), by = 1))
  pre.period <- as.Date(c("2014-01-01", "2014-01-10"))
  post.period <- as.Date(c("2014-01-11", "2014-01-20"))
  impact <- CausalImpact(data, pre.period, post.period, model.args)
  q <- CreateImpactPlot(impact)
  checkEquals(class(q), c("gg", "ggplot"))
  plot(q)

  # Test plot.CausalImpact() generic
  q1 <- CreateImpactPlot(impact)
  q2 <- plot(impact)  # dispatched to plot.CausalImpact()
  checkEquals(q1, q2)

  # Test plotting different metrics
  q1 <- plot(impact)
  q2 <- plot(impact, c("original", "pointwise", "cumulative"))
  q3 <- plot(impact, c("o", "point", "c"))
  checkEquals(q1, q2)
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
  checkEquals(q1, q3)

  # Test different order
  q1 <- plot(impact, c("p", "c"))
  q2 <- plot(impact, c("c", "p"))
  checkTrue(!isTRUE(all.equal(q1, q2)))
}
