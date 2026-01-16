# Fit a classifier to feature matrix using all features or all features by set

Fit a classifier to feature matrix using all features or all features by
set

## Usage

``` r
fit_multivariable_classifier(
  data,
  id_var = "id",
  group_var = "group",
  by_set = FALSE,
  test_method = "gaussprRadial",
  use_balanced_accuracy = FALSE,
  use_k_fold = TRUE,
  num_folds = 10,
  use_empirical_null = FALSE,
  null_testing_method = c("model free shuffles", "null model fits"),
  p_value_method = c("empirical", "gaussian"),
  num_permutations = 100,
  seed = 123
)
```

## Arguments

- data:

  the dataframe containing the raw feature data as calculated by
  [`theft::calculate_features`](https://hendersontrent.github.io/theft/reference/calculate_features.md)

- id_var:

  a string specifying the ID variable to group data on (if one exists).
  Defaults to `"id"`

- group_var:

  a string specifying the grouping variable that the data aggregates to.
  Defaults to `"group"`

- by_set:

  Boolean specifying whether to compute classifiers for each feature
  set. Defaults to `FALSE`

- test_method:

  the algorithm to use for quantifying class separation. Defaults to
  `"gaussprRadial"`

- use_balanced_accuracy:

  a Boolean specifying whether to use balanced accuracy as the summary
  metric for caret model training. Defaults to `FALSE`

- use_k_fold:

  a Boolean specifying whether to use k-fold procedures for generating a
  distribution of classification accuracy estimates. Defaults to `TRUE`

- num_folds:

  an integer specifying the number of folds (train-test splits) to
  perform if `use_k_fold` is set to `TRUE`. Defaults to `10`

- use_empirical_null:

  a Boolean specifying whether to use empirical null procedures to
  compute p-values. Defaults to `FALSE`

- null_testing_method:

  a string specifying the type of statistical method to use to calculate
  p-values. Defaults to `model free shuffles`

- p_value_method:

  a string specifying the method of calculating p-values. Defaults to
  `"empirical"`

- num_permutations:

  an integer specifying the number of class label shuffles to perform if
  `use_empirical_null` is `TRUE`. Defaults to `100`

- seed:

  fixed number for R's random number generator to ensure reproducibility

## Value

an object of class list containing dataframe summaries of the
classification models and a `ggplot` object if `by_set` is `TRUE`

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

fit_multivariable_classifier(featMat,
  id_var = "id",
  group_var = "group",
  by_set = FALSE,
  test_method = "gaussprRadial",
  use_balanced_accuracy = FALSE,
  use_k_fold = TRUE,
  num_folds = 10,
  use_empirical_null = TRUE,
  null_testing_method = "model free shuffles",
  p_value_method = "empirical",
  num_permutations = 100,
  seed = 123)
}
```
