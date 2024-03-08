context("test-calculate_features")

test_that("catch22 feature calculation", {
  
  expect_equal(22 * length(unique(tmp$id)), nrow(feature_matrix))
})

test_that("feasts feature calculation", {
  
  skip_on_cran()
  
  features_feasts <- calculate_features(data = tmp, 
                                        id_var = "id", 
                                        time_var = "timepoint", 
                                        values_var = "values", 
                                        group_var = "process", 
                                        feature_set = "feasts")
  
  expect_equal(43 * length(unique(tmp$id)), nrow(features_feasts))
})

test_that("custom feature calculation", {
  
  features_custom <- calculate_features(data = tmp, 
                                        group_var = "process", 
                                        feature_set = NULL, 
                                        features = list("mean" = mean, "sd" = sd))
  
  expect_equal(2 * length(unique(tmp$id)), nrow(features_custom))
})
