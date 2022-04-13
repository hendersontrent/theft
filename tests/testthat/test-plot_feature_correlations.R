context("test-plot_feature_correlations")

test_that("feature correlations", {
  expect_equal(length(unique(feature_matrix$id)) ^ 2,
               nrow(plot_feature_correlations(feature_matrix, 
                                              is_normalised = FALSE, 
                                              id_var = "id",
                                              names_var = "names", 
                                              values_var = "values", 
                                              method = "RobustSigmoid", 
                                              cor_method = "pearson", 
                                              interactive = FALSE)$data))
})
