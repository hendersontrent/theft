# Helper function to select only the relevant columns for statistical testing

Helper function to select only the relevant columns for statistical
testing

## Usage

``` r
select_stat_cols(data, by_set, metric, hypothesis)
```

## Arguments

- data:

  `data.frame` of classification accuracy results

- by_set:

  `Boolean` specifying whether you want to compare feature sets (if
  `TRUE`) or individual features (if `FALSE`).

- metric:

  `character` denoting the classification performance metric to use in
  statistical testing. Can be one of `"accuracy"`, `"precision"`,
  `"recall"`, `"f1"`. Defaults to `"accuracy"`

- hypothesis:

  `character` denoting whether p-values should be calculated for each
  feature set or feature (depending on `by_set` argument) individually
  relative to the null if `use_null = TRUE` in `tsfeature_classifier`
  through `"null"`, or whether pairwise comparisons between each set or
  feature should be conducted on main model fits only through
  `"pairwise"`.

## Value

object of class `data.frame`

## Author

Trent Henderson
