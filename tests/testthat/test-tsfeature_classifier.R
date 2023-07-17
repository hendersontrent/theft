context("test-tsfeature_classifier")

test_that("default classifier", {
  skip_on_cran()
  feature_classifiers <- tsfeature_classifier(feature_matrix, by_set = FALSE, n_resamples = 5, use_null = TRUE)
  expect_equal(2, length(feature_classifiers))
})

test_that("custom classifier", {
  
  skip_on_cran()
  
  myclassifier <- function(formula, data){
    mod <- e1071::svm(formula, data = data, kernel = "radial", scale = FALSE, probability = TRUE)
  }
  
  feature_classifiers_radial <- tsfeature_classifier(feature_matrix, classifier = myclassifier, by_set = FALSE, n_resamples = 5, use_null = TRUE)
  expect_equal(2, length(feature_classifiers_radial))
})