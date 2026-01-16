# Calculate p-values for feature sets or features relative to an empirical null or each other using resampled t-tests

Calculate p-values for feature sets or features relative to an empirical
null or each other using resampled t-tests

## Usage

``` r
stat_test(
  data,
  iter_data,
  row_id,
  by_set = FALSE,
  hypothesis,
  metric,
  train_test_sizes,
  n_resamples
)
```

## Arguments

- data:

  `data.frame` of raw classification accuracy results

- iter_data:

  `data.frame` containing the values to iterate over for seed and either
  feature name or set name

- row_id:

  `integer` denoting the row ID for `iter_data` to filter to

- by_set:

  `Boolean` specifying whether you want to compare feature sets (if
  `TRUE`) or individual features (if `FALSE`).

- hypothesis:

  `character` denoting whether p-values should be calculated for each
  feature set or feature (depending on `by_set` argument) individually
  relative to the null if `use_null = TRUE` in `tsfeature_classifier`
  through `"null"`, or whether pairwise comparisons between each set or
  feature should be conducted on main model fits only through
  `"pairwise"`.

- metric:

  `character` denoting the classification performance metric to use in
  statistical testing. Can be one of `"accuracy"`, `"precision"`,
  `"recall"`, `"f1"`. Defaults to `"accuracy"`

- train_test_sizes:

  `integer` vector containing the train and test set sample sizes

- n_resamples:

  `integer` denoting the number of resamples that were calculated

## Value

object of class `data.frame`

## Author

Trent Henderson
