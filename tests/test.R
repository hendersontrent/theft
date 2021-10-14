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

library(theft)
library(reticulate)

#------------------- Feature extraction -----------------------------

#outs_all <- calculate_features(data = simData, id_var = "id", time_var = "timepoint", values_var = "values", group_var = "process", feature_set = "all")
outs_22 <- calculate_features(data = simData, id_var = "id", time_var = "timepoint", values_var = "values", group_var = "process", feature_set = "catch22")
#outs_fe <- calculate_features(data = simData, id_var = "id", time_var = "timepoint", values_var = "values", group_var = "process", feature_set = "feasts")
#outs_ts <- calculate_features(data = simData, id_var = "id", time_var = "timepoint", values_var = "values", group_var = "process", feature_set = "tsfeatures")

#reticulate::use_python("~/opt/anaconda3/bin/python", required = TRUE)
#outs_tsfresh <- calculate_features(data = simData, id_var = "id", time_var = "timepoint", values_var = "values", group_var = "process", feature_set = "tsfresh", tsfresh_cleanup = FALSE)
#outs_tsfel <- calculate_features(data = simData, id_var = "id", time_var = "timepoint", values_var = "values", group_var = "process", feature_set = "tsfel")
#outs_kats <- calculate_features(data = simData, id_var = "id", time_var = "timepoint", values_var = "values", group_var = "process", feature_set = "kats")

#------------------- Other package functionality --------------------

# Test 1: Data quality matrix

plot_quality_matrix(outs_22)

# Test 2: Normalisation

normed <- normalise_feature_frame(outs_22, names_var = "names", values_var = "values", method = c("RobustSigmoid"))

# Test 3: Feature matrix

plot_feature_matrix(normed, is_normalised = TRUE, id_var = "id", method = "RobustSigmoid", interactive = FALSE)
plot_feature_matrix(outs_22, is_normalised = FALSE, id_var = "id", method = "RobustSigmoid", interactive = FALSE)
plot_feature_matrix(outs_22, is_normalised = FALSE, id_var = "id", method = "RobustSigmoid", interactive = TRUE)

plot_feature_matrix(outs_22, is_normalised = FALSE, id_var = "id", method = "z-score", interactive = FALSE)
plot_feature_matrix(outs_22, is_normalised = FALSE, id_var = "id", method = "z-score", interactive = TRUE)

# Test 4: Low dimension

plot_low_dimension(outs_22, is_normalised = FALSE, id_var = "id", group_var = "group", plot = TRUE, method = "RobustSigmoid", low_dim_method = "PCA", show_covariance = FALSE)
plot_low_dimension(outs_22, is_normalised = FALSE, id_var = "id", group_var = "group", plot = TRUE, method = "RobustSigmoid", low_dim_method = "PCA", show_covariance = TRUE)
plot_low_dimension(normed, is_normalised = TRUE, id_var = "id", group_var = "group", plot = TRUE, method = "RobustSigmoid", low_dim_method = "PCA")
plot_low_dimension(outs_22, is_normalised = FALSE, id_var = "id", group_var = "group", plot = TRUE, method = "RobustSigmoid", low_dim_method = "t-SNE", perplexity = 5)
plot_low_dimension(normed, is_normalised = TRUE, id_var = "id", group_var = "group", plot = TRUE, method = "RobustSigmoid", low_dim_method = "t-SNE", perplexity = 5)
d1 <- plot_low_dimension(outs_22, is_normalised = FALSE, id_var = "id", group_var = "group", plot = FALSE, method = "RobustSigmoid", low_dim_method = "PCA")
d2 <- plot_low_dimension(outs_22, is_normalised = FALSE, id_var = "id", group_var = "group", plot = FALSE, method = "RobustSigmoid", low_dim_method = "t-SNE", perplexity = 5)

# Test 5: Correlation matrix

plot_correlation_matrix(outs_22, is_normalised = FALSE, id_var = "id", names_var = "names", values_var = "values", method = "RobustSigmoid", cor_method = "pearson", interactive = FALSE)
plot_correlation_matrix(outs_22, is_normalised = FALSE, id_var = "id", names_var = "names", values_var = "values", method = "RobustSigmoid", cor_method = "spearman", interactive = FALSE)
plot_correlation_matrix(outs_22, is_normalised = FALSE, id_var = "id", names_var = "names", values_var = "values", method = "RobustSigmoid", interactive = TRUE)

# Test 6: Feature discrimination

plot_feature_discrimination(outs_22, id_var = "id", group_var = "group", features = "all", normalise = FALSE)
plot_feature_discrimination(outs_22, id_var = "id", group_var = "group", features = c("CO_f1ecac", "CO_FirstMin_ac"), normalise = FALSE)
plot_feature_discrimination(outs_22, id_var = "id", group_var = "group", features = "all", normalise = TRUE, method = "RobustSigmoid")
plot_feature_discrimination(outs_22, id_var = "id", group_var = "group", features = c("CO_f1ecac", "CO_FirstMin_ac"), normalise = TRUE, method = "RobustSigmoid")

# Test 7: Processing hctsa formatted file

d2 <- process_hctsa_file("https://cloudstor.aarnet.edu.au/plus/s/6sRD6IPMJyZLNlN/download")
