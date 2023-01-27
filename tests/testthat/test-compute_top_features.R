context("test-compute_top_features")

test_that("single feature plots", {
  
  skip_on_cran()
  
  expect_equal(10,
               length(unique(compute_top_features(feature_matrix, 
                                                  num_features = 10, 
                                                  normalise_violin_plots = FALSE,
                                                  method = "RobustSigmoid",
                                                  cor_method = "pearson",
                                                  test_method = "gaussprRadial",
                                                  clust_method = "average",
                                                  use_balanced_accuracy = FALSE,
                                                  use_k_fold = FALSE,
                                                  num_folds = 10,
                                                  use_empirical_null =  TRUE,
                                                  null_testing_method = "ModelFreeShuffles",
                                                  num_permutations = 10,
                                                  p_value_method = "empirical",
                                                  pool_empirical_null = FALSE)$ViolinPlots$data$names)))
})

test_that("single feature null method", {
  
  skip_on_cran()
  
  expect_equal(nrow(compute_top_features(feature_matrix, 
                                         num_features = 10, 
                                         normalise_violin_plots = FALSE,
                                         method = "RobustSigmoid",
                                         cor_method = "pearson",
                                         test_method = "gaussprRadial",
                                         clust_method = "average",
                                         use_balanced_accuracy = FALSE,
                                         use_k_fold = FALSE,
                                         num_folds = 10,
                                         use_empirical_null =  TRUE,
                                         null_testing_method = "ModelFreeShuffles",
                                         num_permutations = 10,
                                         p_value_method = "empirical",
                                         pool_empirical_null = FALSE)$ResultsTable),
               nrow(compute_top_features(feature_matrix, 
                                         num_features = 10, 
                                         normalise_violin_plots = FALSE,
                                         method = "RobustSigmoid",
                                         cor_method = "pearson",
                                         test_method = "gaussprRadial",
                                         clust_method = "average",
                                         use_balanced_accuracy = FALSE,
                                         use_k_fold = FALSE,
                                         num_folds = 10,
                                         use_empirical_null =  TRUE,
                                         null_testing_method = "null model fits",
                                         num_permutations = 10,
                                         p_value_method = "gaussian",
                                         pool_empirical_null = FALSE)$ResultsTable))
})

test_that("single feature balanced accuracy", {
  
  skip_on_cran()
  
  expect_equal(7,
               ncol(compute_top_features(feature_matrix, 
                                         num_features = 10, 
                                         normalise_violin_plots = FALSE,
                                         method = "RobustSigmoid",
                                         cor_method = "pearson",
                                         test_method = "gaussprRadial",
                                         clust_method = "average",
                                         use_balanced_accuracy = TRUE,
                                         use_k_fold = FALSE,
                                         num_folds = 10,
                                         use_empirical_null =  TRUE,
                                         null_testing_method = "ModelFreeShuffles",
                                         num_permutations = 10,
                                         p_value_method = "gaussian",
                                         pool_empirical_null = FALSE)$ResultsTable))
  
  expect_equal(5,
               ncol(compute_top_features(feature_matrix, 
                                         num_features = 10, 
                                         normalise_violin_plots = FALSE,
                                         method = "RobustSigmoid",
                                         cor_method = "pearson",
                                         test_method = "gaussprRadial",
                                         clust_method = "average",
                                         use_balanced_accuracy = FALSE,
                                         use_k_fold = FALSE,
                                         num_folds = 10,
                                         use_empirical_null =  TRUE,
                                         null_testing_method = "ModelFreeShuffles",
                                         num_permutations = 10,
                                         p_value_method = "gaussian",
                                         pool_empirical_null = FALSE)$ResultsTable))
})

test_that("single feature binary options", {
  
  t_test_ref <- compute_top_features(feature_matrix, 
                                     num_features = 10, 
                                     normalise_violin_plots = FALSE,
                                     method = "RobustSigmoid",
                                     cor_method = "pearson",
                                     test_method = "t-test",
                                     clust_method = "average",
                                     pool_empirical_null = FALSE)
  
  expect_equal(length(unique(t_test_ref$ResultsTable$feature)),
               length(unique(compute_top_features(feature_matrix, 
                                                  num_features = 10, 
                                                  normalise_violin_plots = FALSE,
                                                  method = "RobustSigmoid",
                                                  cor_method = "pearson",
                                                  test_method = "wilcox",
                                                  clust_method = "average",
                                                  pool_empirical_null = FALSE)$ResultsTable$feature)))
  
  expect_equal(length(unique(t_test_ref$ResultsTable$feature)),
               length(unique(compute_top_features(feature_matrix, 
                                                  num_features = 10, 
                                                  normalise_violin_plots = FALSE,
                                                  method = "RobustSigmoid",
                                                  cor_method = "pearson",
                                                  test_method = "BinomialLogistic",
                                                  clust_method = "average",
                                                  pool_empirical_null = FALSE)$ResultsTable$feature)))
})
