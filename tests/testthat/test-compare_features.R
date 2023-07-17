context("test-compare_features")

test_that("null test statistics", {
  skip_on_cran()
  feature_classifiers_null <- tsfeature_classifier(feature_matrix, by_set = FALSE, n_resamples = 5, use_null = TRUE)
  feature_vs_null <- compare_features(feature_classifiers_null, by_set = FALSE, hypothesis = "null")
  expect_equal(22, nrow(feature_vs_null))
})

test_that("pairwise test statistics", {
  skip_on_cran()
  feature_classifiers_pairwise <- tsfeature_classifier(feature_matrix, by_set = FALSE, n_resamples = 5, use_null = TRUE)
  pairwise_features <- compare_features(feature_classifiers_pairwise, by_set = FALSE, hypothesis = "pairwise")
  expect_equal(231, nrow(pairwise_features))
})