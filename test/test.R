#---------------------------------------
# This script sets out to test the core
# functionality of the {theft} package.
#
# NOTE: This should be converted to the
# {testthat} package for full proper
# unit tests
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 07 April 2021
#---------------------------------------

library(dplyr)
library(theft)

# Retrieve some data

d <- tsibbledata::aus_retail %>%
  filter(State == "New South Wales")

#------------------- Feature extraction -----------------------------

outs_all <- calculate_features(data = d, id_var = "Industry", time_var = "Month", values_var = "Turnover", feature_set = "all")
outs_fe <- calculate_features(data = d, id_var = "Industry", time_var = "Month", values_var = "Turnover", feature_set = "feasts")
outs_ts <- calculate_features(data = d, id_var = "Industry", time_var = "Month", values_var = "Turnover", feature_set = "tsfeatures")
outs_22 <- calculate_features(data = d, id_var = "Industry", time_var = "Month", values_var = "Turnover", feature_set = "catch22")

#------------------- Other package functionality --------------------

# Test 1: Data quality matrix

plot_quality_matrix(outs_ts)

# Test 2: Normalisation

ar_process <- 1 + 0.5 * 1:1000 + arima.sim(list(ma = 0.5), n = 1000)
vec1 <- normalise_feature_vector(ar_process, method = "z-score")
vec2 <- normalise_feature_vector(ar_process, method = "Sigmoid")
vec3 <- normalise_feature_vector(ar_process, method = "RobustSigmoid")
vec4 <- normalise_feature_vector(ar_process, method = "MinMax")
vec5 <- normalise_feature_vector(ar_process, method = "MeanSubtract")

normed <- normalise_feature_frame(outs_ts, names_var = "names", values_var = "values", method = c("RobustSigmoid"))
normed1 <- normalise_feature_frame(outs_ts, names_var = "names", values_var = "values", method = c("MinMax"))

# Test 3: Feature matrix

plot_feature_matrix(normed, is_normalised = TRUE, id_var = "id", method = "RobustSigmoid")
plot_feature_matrix(outs_ts, is_normalised = FALSE, id_var = "id", method = "MinMax")

# Test 4: PCA

plot_low_dimension(outs_ts, is_normalised = FALSE, id_var = "id", group_var = NULL, plot = TRUE, method = "RobustSigmoid")
plot_low_dimension(normed, is_normalised = TRUE, id_var = "id", group_var = NULL, plot = TRUE, method = "RobustSigmoid")
d1 <- plot_low_dimension(outs_ts, is_normalised = FALSE, id_var = "id", group_var = NULL, plot = FALSE, method = "RobustSigmoid")

# Test 5: Connectivity matrix

plot_connectivity_matrix(outs_ts, id_var = "id", names_var = "names", values_var = "values")
