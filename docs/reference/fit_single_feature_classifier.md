# Fit a classifier to feature matrix to extract top performers

Fit a classifier to feature matrix to extract top performers

## Usage

``` r
fit_single_feature_classifier(
  data,
  test_method = "gaussprRadial",
  use_balanced_accuracy = FALSE,
  use_k_fold = FALSE,
  num_folds = 10,
  use_empirical_null = FALSE,
  null_testing_method = c("ModelFreeShuffles", "NullModelFits"),
  p_value_method = c("empirical", "gaussian"),
  num_permutations = 50,
  pool_empirical_null = FALSE,
  seed = 123
)
```

## Arguments

- data:

  the `data.frame` containing the raw feature matrix

- test_method:

  `character` specifying the algorithm to use for quantifying class
  separation. Defaults to `"gaussprRadial"`. Should be either
  `"t-test"`, `"wilcox"`, or `"binomial logistic"` for two-class
  problems to obtain exact statistics, or a valid `caret` classification
  model for everything else

- use_balanced_accuracy:

  `Boolean` specifying whether to use balanced accuracy as the summary
  metric for caret model training. Defaults to `FALSE`

- use_k_fold:

  `Boolean` specifying whether to use k-fold procedures for generating a
  distribution of classification accuracy estimates if a `caret` model
  is specified for `test_method`. Defaults to ` FALSE`

- num_folds:

  `integer` specifying the number of k-folds to perform if `use_k_fold`
  is set to `TRUE`. Defaults to `10`

- use_empirical_null:

  `Boolean` specifying whether to use empirical null procedures to
  compute p-values if a `caret` model is specified for `test_method`.
  Defaults to `FALSE`

- null_testing_method:

  `character` specifying the type of statistical method to use to
  calculate p-values. Defaults to `model free shuffles`

- p_value_method:

  `character` specifying the method of calculating p-values. Defaults to
  `"empirical"`

- num_permutations:

  `integer` specifying the number of class label shuffles to perform if
  `use_empirical_null` is `TRUE`. Defaults to `50`

- pool_empirical_null:

  `Boolean` specifying whether to use the pooled empirical null
  distribution of all features or each features' individual empirical
  null distribution if a `caret` model is specified for `test_method`
  use_empirical_null is `TRUE`. Defaults to `FALSE`

- seed:

  `integer` denoting a fixed number for R's random number generator to
  ensure reproducibility. Defaults to `123`

## Value

an object of class `data.frame`

## Author

Trent Henderson
