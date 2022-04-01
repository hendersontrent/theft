context("test-fit_multivariable_classifier")

feature_matrix <- calculate_features(data = theft::simData, 
                                     id_var = "id", 
                                     time_var = "timepoint", 
                                     values_var = "values", 
                                     group_var = "process", 
                                     feature_set = c("catch22", "feasts"))

test_that("graphics", {
  expect_equal(2, 
               length(fit_multivariable_classifier(feature_matrix, id_var = "id", group_var = "group",
                                                   by_set = FALSE, test_method = "svmLinear", use_balanced_accuracy = TRUE,
                                                   use_k_fold = FALSE, num_folds = 10, 
                                                   use_empirical_null = TRUE, null_testing_method = "model free shuffles",
                                                   p_value_method = "empirical", num_permutations = 100)))
  
  expect_equal(3, 
               length(fit_multivariable_classifier(feature_matrix, id_var = "id", group_var = "group",
                                                   by_set = TRUE, test_method = "svmLinear", use_balanced_accuracy = TRUE,
                                                   use_k_fold = FALSE, num_folds = 10, 
                                                   use_empirical_null = TRUE, null_testing_method = "model free shuffles",
                                                   p_value_method = "empirical", num_permutations = 100)))
})

test_that("balanced accuracy", {
  expect_equal(7, 
               ncol(fit_multivariable_classifier(feature_matrix, id_var = "id", group_var = "group",
                                                   by_set = TRUE, test_method = "svmLinear", use_balanced_accuracy = TRUE,
                                                   use_k_fold = FALSE, num_folds = 10, 
                                                   use_empirical_null = TRUE, null_testing_method = "model free shuffles",
                                                   p_value_method = "empirical", num_permutations = 100)$TestStatistics))
  
  expect_equal(7, 
               ncol(fit_multivariable_classifier(feature_matrix, id_var = "id", group_var = "group",
                                                 by_set = TRUE, test_method = "svmLinear", use_balanced_accuracy = TRUE,
                                                 use_k_fold = FALSE, num_folds = 10, 
                                                 use_empirical_null = TRUE, null_testing_method = "model free shuffles",
                                                 p_value_method = "empirical", num_permutations = 100)$RawClassificationResults))
  
  expect_equal(5, 
               ncol(fit_multivariable_classifier(feature_matrix, id_var = "id", group_var = "group",
                                                 by_set = TRUE, test_method = "svmLinear", use_balanced_accuracy = FALSE,
                                                 use_k_fold = FALSE, num_folds = 10, 
                                                 use_empirical_null = TRUE, null_testing_method = "model free shuffles",
                                                 p_value_method = "empirical", num_permutations = 100)$TestStatistics))
  
  expect_equal(6, 
               ncol(fit_multivariable_classifier(feature_matrix, id_var = "id", group_var = "group",
                                                 by_set = TRUE, test_method = "svmLinear", use_balanced_accuracy = FALSE,
                                                 use_k_fold = FALSE, num_folds = 10, 
                                                 use_empirical_null = TRUE, null_testing_method = "model free shuffles",
                                                 p_value_method = "empirical", num_permutations = 100)$RawClassificationResults))
})

test_that("null model fits", {
  expect_equal(nrow(fit_multivariable_classifier(feature_matrix, 
                                                 id_var = "id", group_var = "group",
                                                 by_set = FALSE, test_method = "svmLinear", use_balanced_accuracy = FALSE,
                                                 use_k_fold = TRUE, num_folds = 10, 
                                                 use_empirical_null = TRUE, null_testing_method = "model free shuffles",
                                                 p_value_method = "empirical", num_permutations = 10)$RawClassificationResults),
               nrow(fit_multivariable_classifier(feature_matrix, 
                                                 id_var = "id", group_var = "group",
                                                 by_set = FALSE, test_method = "svmLinear", use_balanced_accuracy = FALSE,
                                                 use_k_fold = TRUE, num_folds = 10, 
                                                 use_empirical_null = TRUE, null_testing_method = "null model fits",
                                                 p_value_method = "empirical", num_permutations = 10)$RawClassificationResults))
})

test_that("p-value method", {
  expect_equal(nrow(fit_multivariable_classifier(feature_matrix, 
                                                 id_var = "id", group_var = "group",
                                                 by_set = FALSE, test_method = "svmLinear", 
                                                 use_balanced_accuracy = FALSE, use_k_fold = FALSE, num_folds = 10, 
                                                 use_empirical_null = TRUE, null_testing_method = "model free shuffles",
                                                 p_value_method = "empirical", num_permutations = 100)$TestStatistics),
               nrow(fit_multivariable_classifier(feature_matrix, 
                                                 id_var = "id", group_var = "group",
                                                 by_set = FALSE, test_method = "svmLinear", use_balanced_accuracy = FALSE,
                                                 use_k_fold = FALSE, num_folds = 10, 
                                                 use_empirical_null = TRUE, null_testing_method = "model free shuffles",
                                                 p_value_method = "gaussian", num_permutations = 100)$TestStatistics))
})
