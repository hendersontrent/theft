context("test-fit_multi_feature_classifier")

test_that("multi feature graphics", {
  
  skip_on_cran()
  
  expect_equal(2, 
               length(fit_multi_feature_classifier(feature_matrix, id_var = "id", group_var = "group",
                                                   by_set = FALSE, test_method = "gaussprRadial", use_balanced_accuracy = TRUE,
                                                   use_k_fold = FALSE, num_folds = 10, 
                                                   use_empirical_null = TRUE, null_testing_method = "model free shuffles",
                                                   p_value_method = "empirical", num_permutations = 10)))
  
  expect_equal(3, 
               length(fit_multi_feature_classifier(feature_matrix, id_var = "id", group_var = "group",
                                                   by_set = TRUE, test_method = "gaussprRadial", use_balanced_accuracy = TRUE,
                                                   use_k_fold = FALSE, num_folds = 10, 
                                                   use_empirical_null = TRUE, null_testing_method = "model free shuffles",
                                                   p_value_method = "empirical", num_permutations = 10)))
})

test_that("multi feature balanced accuracy", {
  
  skip_on_cran()
  
  expect_equal(7, 
               ncol(fit_multi_feature_classifier(feature_matrix, id_var = "id", group_var = "group",
                                                   by_set = TRUE, test_method = "gaussprRadial", use_balanced_accuracy = TRUE,
                                                   use_k_fold = FALSE, num_folds = 10, 
                                                   use_empirical_null = TRUE, null_testing_method = "model free shuffles",
                                                   p_value_method = "empirical", num_permutations = 10)$TestStatistics))
  
  expect_equal(5, 
               ncol(fit_multi_feature_classifier(feature_matrix, id_var = "id", group_var = "group",
                                                 by_set = TRUE, test_method = "gaussprRadial", use_balanced_accuracy = FALSE,
                                                 use_k_fold = FALSE, num_folds = 10, 
                                                 use_empirical_null = TRUE, null_testing_method = "model free shuffles",
                                                 p_value_method = "empirical", num_permutations = 10)$TestStatistics))
})

test_that("multi feature null model fits", {
  
  skip_on_cran()
  
  expect_equal(nrow(fit_multi_feature_classifier(feature_matrix, 
                                                 id_var = "id", group_var = "group",
                                                 by_set = FALSE, test_method = "gaussprRadial", use_balanced_accuracy = FALSE,
                                                 use_k_fold = TRUE, num_folds = 10, 
                                                 use_empirical_null = TRUE, null_testing_method = "model free shuffles",
                                                 p_value_method = "empirical", num_permutations = 10)$RawClassificationResults),
               nrow(fit_multi_feature_classifier(feature_matrix, 
                                                 id_var = "id", group_var = "group",
                                                 by_set = FALSE, test_method = "gaussprRadial", use_balanced_accuracy = FALSE,
                                                 use_k_fold = TRUE, num_folds = 10, 
                                                 use_empirical_null = TRUE, null_testing_method = "null model fits",
                                                 p_value_method = "empirical", num_permutations = 10)$RawClassificationResults))
})
