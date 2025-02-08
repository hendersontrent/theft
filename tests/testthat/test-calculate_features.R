context("test-calculate_features")

test_that("catch22 feature calculation", {
  
  expect_equal(22 * length(unique(theft::simData$id)), nrow(feature_matrix))
})

test_that("feasts feature calculation", {
  
  skip_on_cran()
  
  features_feasts <- calculate_features(data = theft::simData, 
                                        feature_set = "feasts")
  
  expect_equal(43 * length(unique(theft::simData$id)), nrow(features_feasts))
})

test_that("custom feature calculation", {
  
  features_custom <- calculate_features(data = theft::simData, 
                                        feature_set = NULL, 
                                        features = list("mean" = mean, "sd" = sd))
  
  expect_equal(2 * length(unique(theft::simData$id)), nrow(features_custom))
})
