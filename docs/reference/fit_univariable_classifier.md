# Fit a classifier to feature matrix to extract top performers

Fit a classifier to feature matrix to extract top performers

## Usage

``` r
fit_univariable_classifier(
  data,
  id_var = "id",
  group_var = "group",
  test_method = "gaussprRadial",
  use_balanced_accuracy = FALSE,
  use_k_fold = FALSE,
  num_folds = 10,
  use_empirical_null = FALSE,
  null_testing_method = c("model free shuffles", "null model fits"),
  p_value_method = c("empirical", "gaussian"),
  num_permutations = 50,
  pool_empirical_null = FALSE,
  seed = 123,
  return_raw_estimates = FALSE
)
```

## Arguments

- data:

  the dataframe containing the raw feature matrix

- id_var:

  a string specifying the ID variable to group data on (if one exists).
  Defaults to `"id"`

- group_var:

  a string specifying the grouping variable that the data aggregates to.
  Defaults to `"group"`

- test_method:

  the algorithm to use for quantifying class separation. Defaults to
  `"gaussprRadial"`. Should be either `"t-test"`, `"wilcox"`, or
  `"binomial logistic"` for two-class problems to obtain exact
  statistics, or a valid `caret` classification model for everything
  else

- use_balanced_accuracy:

  a Boolean specifying whether to use balanced accuracy as the summary
  metric for caret model training. Defaults to `FALSE`

- use_k_fold:

  a Boolean specifying whether to use k-fold procedures for generating a
  distribution of classification accuracy estimates if a `caret` model
  is specified for `test_method`. Defaults to ` FALSE`

- num_folds:

  an integer specifying the number of k-folds to perform if `use_k_fold`
  is set to `TRUE`. Defaults to `10`

- use_empirical_null:

  a Boolean specifying whether to use empirical null procedures to
  compute p-values if a `caret` model is specified for `test_method`.
  Defaults to `FALSE`

- null_testing_method:

  a string specifying the type of statistical method to use to calculate
  p-values. Defaults to `model free shuffles`

- p_value_method:

  a string specifying the method of calculating p-values. Defaults to
  `"empirical"`

- num_permutations:

  an integer specifying the number of class label shuffles to perform if
  `use_empirical_null` is `TRUE`. Defaults to `50`

- pool_empirical_null:

  a Boolean specifying whether to use the pooled empirical null
  distribution of all features or each features' individual empirical
  null distribution if a `caret` model is specified for `test_method`
  use_empirical_null is `TRUE`. Defaults to `FALSE`

- seed:

  fixed number for R's random number generator to ensure reproducibility

- return_raw_estimates:

  a Boolean (for testing purposes only -- will break
  `compute_top_features`!!) specifying whether to return the raw main
  and null model results

## Value

an object of class dataframe containing results

## Author

Trent Henderson

## Examples

``` r
if (FALSE) {
featMat <- calculate_features(data = simData, 
  id_var = "id", 
  time_var = "timepoint", 
  values_var = "values", 
  group_var = "process", 
  feature_set = "catch22")
  
fit_univariable_classifier(featMat,
  id_var = "id",
  group_var = "group",
  test_method = "linear svm",
  use_balanced_accuracy = FALSE,
  use_k_fold = TRUE,
  num_folds = 10,
  use_empirical_null = TRUE,
  null_testing_method = "model free shuffles",
  p_value_method = "empirical",
  num_permutations = 50,
  pool_empirical_null = FALSE,
  seed = 123,
  return_raw_estimates = FALSE) 
}
```
