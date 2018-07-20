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

# Unit test for CausalImpact imports.
#
# Checks whether `CausalImpact()` can be called without attaching the
# CausalImpact package. The aim of this test is to check whether CausalImpact
# correctly imports all functions needed.
#
# We therefore intentionally do not call `library(CausalImpact)` in this test.
#
# Author: alhauser@google.com (Alain Hauser)


library(testthat)

# Create a data set with one explanatory variable.
set.seed(1)
x1 <- 100 + arima.sim(model = list(ar = 0.999), n = 100)
y <- 1.2 * x1 + rnorm(100)
y[71:100] <- y[71:100] + 10
data <- cbind(y, x1)

# Test that a `CausalImpact` object is created when the package is not loaded.
impact <- CausalImpact::CausalImpact(data, c(1, 70), c(71, 100))
expect_is(impact, "CausalImpact")

# Request dynamic regression to check that `GammaPrior()` is correctly imported.
impact <- CausalImpact::CausalImpact(
    data, c(1, 70), c(71, 100), model.args = list(dynamic.regression = TRUE))
expect_is(impact, "CausalImpact")
