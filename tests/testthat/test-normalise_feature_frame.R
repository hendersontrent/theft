context("test-normalise_feature_frame")

test_that("dataframe normalisation", {
  expect_equal(nrow(feature_matrix), 
               nrow(normalise_feature_frame(feature_matrix, names_var = "names", values_var = "values", method = c("RobustSigmoid"))))
  
  expect_equal(nrow(feature_matrix), 
               nrow(normalise_feature_frame(feature_matrix, names_var = "names", values_var = "values", method = c("Sigmoid"))))
  
  expect_equal(nrow(feature_matrix), 
               nrow(normalise_feature_frame(feature_matrix, names_var = "names", values_var = "values", method = c("z-score"))))
  
  expect_equal(nrow(feature_matrix), 
               nrow(normalise_feature_frame(feature_matrix, names_var = "names", values_var = "values", method = c("MinMax"))))
})

