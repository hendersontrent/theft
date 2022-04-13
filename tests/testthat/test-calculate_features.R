context("test-calculate_features")

tmp <- theft::simData
tmp <- tmp[tmp$process %in% c("Gaussian Noise", "AR(1)"), ]

test_that("catch22 feature calculation", {
  
  features_catch22 <- calculate_features(data = tmp, 
                                         id_var = "id", 
                                         time_var = "timepoint", 
                                         values_var = "values", 
                                         group_var = "process", 
                                         feature_set = "catch22", 
                                         catch24 = FALSE)
  
  expect_equal(22 * length(unique(tmp$id)), 
               nrow(features_catch22))
  
  features_catch24 <- calculate_features(data = tmp, 
                                         id_var = "id", 
                                         time_var = "timepoint", 
                                         values_var = "values", 
                                         group_var = "process", 
                                         feature_set = "catch22", 
                                         catch24 = TRUE)
  
  expect_equal(24 * length(unique(tmp$id)), 
               nrow(features_catch24))
  
  # sub_catch22 <- features_catch22[features_catch22$id == unique(tmp$id)[1], ]
  # 
  # expect_equal(sub_catch22$values,
  #                   c(0.344964114375865, -0.451182445218351, 1, 1, 0.0687661181462487, 
  #                     -0.337971798248684, 0.955357142857143, 9, 0.0106292517006803, 7, 
  #                     0.0991329801853152, 5, 0.25, -0.115044247787611, 0.398230088495575, 
  #                     0.239967247041806, 6, 2.18861051546043, 0.162162162162162, 
  #                     0.702702702702703, 1.570796326795, 1.14432818458087),
  #              ignore_attr = TRUE)
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
  
  # sub_feasts <- features_feasts[features_feasts$id == unique(tmp$id)[1], ]
  # 
  # expect_equal(sub_feasts$values,
  #                   c(0.072296444522295, 0.000120951260469623, -1.17048336174267, 
  #                     -0.448148244799823, -0.0599597956226268, 0.119892570623111, 
  #                     0.00930417330152654, 0.0936461319341435, -0.510181541689284, 
  #                     0.368022825410556, -0.66891833455421, 0.618467095765786, 
  #                     0.0288257547334132, 0.452315378731658, 1.02800341843053, 0, 
  #                     330.021488169417, 0, 0, 1.05331366470291, 0.24888709987701, 
  #                     0.1, -10.3968865852375, 0.01, 0, 0, 0.00978214341320684, 
  #                     0.921213922169577, 0.0100441651117749, 0.920169205985872, 
  #                     0.200199908738181, 0.107573804145576, 0.965791896356446, 53, 
  #                     1.41122524015788, 30, 0.547764203976799, 38, 1, 63, 4, 
  #                     0.500045830129239, 0.0896141376556983),
  #              ignore_attr = TRUE)
})
