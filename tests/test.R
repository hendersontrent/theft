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
library(tsibbledata)
library(reticulate)

# Retrieve some data

d <- tsibbledata::aus_retail %>%
  filter(State == "New South Wales")

#------------------- Feature extraction -----------------------------

#outs_all <- calculate_features(data = d, id_var = "Industry", time_var = "Month", values_var = "Turnover", group_var = "State", feature_set = "all")
#outs_22 <- calculate_features(data = d, id_var = "Industry", time_var = "Month", values_var = "Turnover", group_var = "State", feature_set = "catch22")
#outs_fe <- calculate_features(data = d, id_var = "Industry", time_var = "Month", values_var = "Turnover", group_var = "State", feature_set = "feasts")
outs_ts <- calculate_features(data = d, id_var = "Industry", time_var = "Month", values_var = "Turnover", group_var = "State", feature_set = "tsfeatures")

#reticulate::use_python("~/opt/anaconda3/bin/python", required = TRUE)
#outs_tsfresh <- calculate_features(data = d, id_var = "Industry", time_var = "Month", values_var = "Turnover", group_var = "State", feature_set = "tsfresh")
#outs_tsfel <- calculate_features(data = d, id_var = "Industry", time_var = "Month", values_var = "Turnover", group_var = "State", feature_set = "tsfel")

#------------------- Other package functionality --------------------

# Test 1: Data quality matrix

plot_quality_matrix(outs_ts)

# Test 2: Normalisation

normed <- normalise_feature_frame(outs_ts, names_var = "names", values_var = "values", method = c("RobustSigmoid"))

# Test 3: Feature matrix

plot_feature_matrix(normed, is_normalised = TRUE, id_var = "id", method = "RobustSigmoid", interactive = FALSE)
plot_feature_matrix(outs_ts, is_normalised = FALSE, id_var = "id", method = "RobustSigmoid", interactive = FALSE)
plot_feature_matrix(outs_ts, is_normalised = FALSE, id_var = "id", method = "RobustSigmoid", interactive = TRUE)

# Test 4: Low dimension

plot_low_dimension(outs_ts, is_normalised = FALSE, id_var = "id", group_var = NULL, plot = TRUE, method = "RobustSigmoid", low_dim_method = "PCA")
plot_low_dimension(normed, is_normalised = TRUE, id_var = "id", group_var = NULL, plot = TRUE, method = "RobustSigmoid", low_dim_method = "PCA")
plot_low_dimension(outs_ts, is_normalised = FALSE, id_var = "id", group_var = NULL, plot = TRUE, method = "RobustSigmoid", low_dim_method = "t-SNE", perplexity = 2)
plot_low_dimension(normed, is_normalised = TRUE, id_var = "id", group_var = NULL, plot = TRUE, method = "RobustSigmoid", low_dim_method = "t-SNE", perplexity = 2)
d1 <- plot_low_dimension(outs_ts, is_normalised = FALSE, id_var = "id", group_var = NULL, plot = FALSE, method = "RobustSigmoid", low_dim_method = "t-SNE", perplexity = 2)

# Test 5: Connectivity matrix

plot_connectivity_matrix(outs_ts, is_normalised = FALSE, id_var = "id", names_var = "names", values_var = "values", method = "RobustSigmoid", interactive = FALSE)
plot_connectivity_matrix(outs_ts, is_normalised = FALSE, id_var = "id", names_var = "names", values_var = "values", method = "RobustSigmoid", interactive = TRUE)
