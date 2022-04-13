context("test-plot_low_dimension")

test_that("low dim data matrices", {
  expect_equal(length(unique(feature_matrix$id)), 
               nrow(plot_low_dimension(feature_matrix, 
                                       is_normalised = FALSE, 
                                       id_var = "id", 
                                       group_var = "group", 
                                       plot = TRUE, 
                                       method = "RobustSigmoid", 
                                       low_dim_method = "PCA", 
                                       show_covariance = FALSE)$data))
  
  expect_equal(length(unique(feature_matrix$id)), 
               nrow(plot_low_dimension(feature_matrix, 
                                       is_normalised = FALSE, 
                                       id_var = "id", 
                                       group_var = "group", 
                                       plot = TRUE, 
                                       method = "RobustSigmoid", 
                                       low_dim_method = "t-SNE", 
                                       perplexity = 5,
                                       show_covariance = FALSE)$data))
})

test_that("t-sne perplexity", {
  expect_error(plot_low_dimension(feature_matrix, 
                                  is_normalised = FALSE, 
                                  id_var = "id", 
                                  group_var = "group", 
                                  plot = TRUE, 
                                  method = "RobustSigmoid", 
                                  low_dim_method = "t-SNE", 
                                  perplexity = (length(feature_matrix$id) + 1),
                                  show_covariance = FALSE),
               "perplexity must be < number of unique IDs.",
               fixed = TRUE)
})
