# Perform fast and simple univariate feature selection based on an output vector using analysis of variance or correlation

Perform fast and simple univariate feature selection based on an output
vector using analysis of variance or correlation

## Usage

``` r
select_k_best(
  data,
  k = floor(length(unique(data[[1]]$names))/2),
  outputs = NULL
)
```

## Arguments

- data:

  the `feature_calculations` object containing the raw feature matrix
  produced by `calculate_features`

- k:

  `integer` denoting the number of features to retain. Defaults to half
  the length of the unique features available in `data`

- outputs:

  `data.frame` containing output data if it was not included in the
  `group` column initially when running `calculate_features`. Should
  have 2 columns where the first is an ID variable that can be joined to
  the `id` column of `data` and the second the output of interest (i.e.,
  a 'y' variable). If a character or factor, classification is assumed,
  and if numeric, regression is assumed. Defaults to `NULL` which
  assumes the `group` variable exists in `data`

## Value

object of class `feature_calculations` that contains the summary
statistics for each feature that was retained

## Author

Trent Henderson
