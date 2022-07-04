context("test-plot_all_features")

normed <- normalise_feature_frame(feature_matrix, 
                                  names_var = "names", 
                                  values_var = "values", 
                                  method = c("RobustSigmoid"))

test_that("feature matrix plot", {
  expect_equal(nrow(feature_matrix),
               nrow(plot_all_features(feature_matrix, 
                                      id_var = "id", 
                                      clust_method = "average",
                                      interactive = FALSE)$data))
  
  expect_equal(nrow(plot_all_features(normed, 
                                      id_var = "id", 
                                      clust_method = "average",
                                      interactive = FALSE)$data),
               nrow(plot_all_features(feature_matrix, 
                                      id_var = "id", 
                                      clust_method = "average",
                                      interactive = FALSE)$data))
})

