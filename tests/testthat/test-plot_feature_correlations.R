context("test-plot_feature_correlations")

test_that("feature correlations", {
  expect_equal(length(unique(feature_matrix$id)) ^ 2,
               nrow(plot_feature_correlations(feature_matrix, 
                                              id_var = "id",
                                              names_var = "names", 
                                              values_var = "values", 
                                              cor_method = "pearson", 
                                              clust_method = "average",
                                              interactive = FALSE)$data))
})
