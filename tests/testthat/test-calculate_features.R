context("test-calculate_features")

test_that("catch22 feature calculation", {
  
  expect_equal(22 * length(unique(tmp$id)), 
               nrow(feature_matrix))
  
  features_catch24 <- calculate_features(data = tmp, 
                                         id_var = "id", 
                                         time_var = "timepoint", 
                                         values_var = "values", 
                                         group_var = "process", 
                                         feature_set = "catch22", 
                                         catch24 = TRUE)
  
  expect_equal(24 * length(unique(tmp$id)), 
               nrow(features_catch24))
})

test_that("feasts feature calculation", {
  
  skip_on_cran()
  
  features_feasts <- calculate_features(data = tmp, 
                                        id_var = "id", 
                                        time_var = "timepoint", 
                                        values_var = "values", 
                                        group_var = "process", 
                                        feature_set = "feasts")
  
  expect_equal(43 * length(unique(tmp$id)), 
               nrow(features_feasts))
})
