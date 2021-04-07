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

#------------------- Core package functionality ---------------------

outs <- calculate_features(tsibbledata::ausretail, 
                           id_var = "Industry", group_var = "State",
                           time_var = "Month", value_var = "Turnover", 
                           feature_set = "all")

outsNormed <- normalise_feature_frame(outs, names_var = "names", values_var = "values", method = "RobustSigmoid")

#------------------- Other package functionality --------------------

#.....
# NOTE: If above function fails, then catch22::catch22_all is broken
#.....

# Test 1: Data quality matrix

plot_quality_matrix(outs)

# Test 2: Normalisation

test_scaler <- function(method = c("z-score", "Sigmoid", "RobustSigmoid", "MinMax", "MeanSubtract")){
  df <- outs %>%
    group_by(names) %>%
    mutate(values = normalise_catch(values, method = method)) %>%
    ungroup()
  
  return(df)
}

scale_test_z <- test_scaler(method = "z-score")
scale_test_sigmoid <- test_scaler(method = "Sigmoid")
scale_test_rsigmoid <- test_scaler(method = "RobustSigmoid")
scale_test_minmax <- test_scaler(method = "MinMax")
scale_test_meansub <- test_scaler(method = "MeanSubtract")

# Test 3: Feature matrix

plot_feature_matrix(outs, is_normalised = FALSE, id_var = "id", method = "RobustSigmoid")

# Test 4: PCA

# Grouped

plot_low_dimension(outs, is_normalised = FALSE, id_var = "id", group_var = "Keywords", plot = TRUE, method = "RobustSigmoid")
d <- plot_low_dimension(outs, is_normalised = FALSE, id_var = "id", group_var = "Keywords", plot = FALSE, method = "RobustSigmoid")

# Ungrouped

plot_low_dimension(outs, is_normalised = FALSE, id_var = "id", group_var = NULL, plot = TRUE, method = "RobustSigmoid")
d1 <- plot_low_dimension(outs, is_normalised = FALSE, id_var = "id", group_var = NULL, plot = FALSE, method = "RobustSigmoid")
