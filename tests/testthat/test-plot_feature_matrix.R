context("test-plot_feature_matrix")

normed <- normalise_feature_frame(feature_matrix, 
                                  names_var = "names", 
                                  values_var = "values", 
                                  method = c("RobustSigmoid"))

test_that("feature matrix plot", {
  expect_equal(nrow(feature_matrix),
               nrow(plot_feature_matrix(feature_matrix, 
                                        is_normalised = FALSE, 
                                        id_var = "id", 
                                        method = "RobustSigmoid", 
                                        interactive = FALSE)$data))
  
  expect_equal(nrow(plot_feature_matrix(normed, 
                                        is_normalised = TRUE, 
                                        id_var = "id", 
                                        method = "RobustSigmoid", 
                                        interactive = FALSE)$data),
               nrow(plot_feature_matrix(feature_matrix, 
                                        is_normalised = FALSE, 
                                        id_var = "id", 
                                        method = "RobustSigmoid", 
                                        interactive = FALSE)$data))
})

