context("test-plot_feature_correlations")

feature_matrix <- calculate_features(data = theft::simData, 
                                     id_var = "id", 
                                     time_var = "timepoint", 
                                     values_var = "values", 
                                     group_var = "process", 
                                     feature_set = "catch22")

test_that("feature correlations", {
  expect_equal(length(unique(feature_matrix$names)) ^ 2,
               nrow(plot_feature_correlations(feature_matrix, 
                                              is_normalised = FALSE, 
                                              id_var = "id",
                                              names_var = "names", 
                                              values_var = "values", 
                                              method = "RobustSigmoid", 
                                              cor_method = "pearson", 
                                              interactive = FALSE)$data))
})
