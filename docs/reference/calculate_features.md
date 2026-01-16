# Compute features on an input time series dataset

Compute features on an input time series dataset

## Usage

``` r
calculate_features(
  data,
  feature_set = c("catch22", "feasts", "tsfeatures", "kats", "tsfresh", "tsfel", "hctsa",
    "quantiles", "moments"),
  features = NULL,
  catch24 = FALSE,
  tsfresh_cleanup = FALSE,
  use_compengine = FALSE,
  seed = 123,
  z_score = FALSE,
  n_jobs = 0,
  warn = TRUE
)
```

## Arguments

- data:

  `tbl_ts` containing the time series data

- feature_set:

  `character` or `vector` of `character` denoting the set of time-series
  features to calculate. Can be one of `"catch22"`, `"feasts"`,
  `"tsfeatures"`, `"tsfresh"`, `"tsfel"`, `"kats"`, `"quantiles"`, and
  or `"moments"`

- features:

  named `list` containing a set of user-supplied functions to calculate
  on `data`. Each function should take a single argument which is the
  time series. Defaults to `NULL` for no manually-specified features.
  Each list entry must have a name as `calculate_features` looks for
  these to name the features. If you don't want to use the existing
  feature sets and only compute those passed to `features`, set
  `feature_set = NULL`

- catch24:

  `Boolean` specifying whether to compute `catch24` in addition to
  `catch22` if `catch22` is one of the feature sets selected. Defaults
  to `FALSE`

- tsfresh_cleanup:

  `Boolean` specifying whether to use the in-built `tsfresh` relevant
  feature filter or not. Defaults to `FALSE`

- use_compengine:

  `Boolean` specifying whether to use the `"compengine"` features in
  `tsfeatures`. Defaults to `FALSE` to provide immense computational
  efficiency benefits

- seed:

  `integer` denoting a fixed number for R's random number generator to
  ensure reproducibility. Defaults to `123`

- z_score:

  `Boolean` specifying whether to z-score the time-series before
  computing features. Defaults to `FALSE`

- n_jobs:

  `integer` denoting the number of parallel processes to use if
  `"tsfresh"` or `"tsfel"` are specified in `"feature_set"`. Defaults to
  `0` for no parallelisation

- warn:

  `Boolean` specifying whether to produce warnings from feature set
  packages. Defaults to `TRUE`

## Value

object of class `feature_calculations` that contains the summary
statistics for each feature

## Author

Trent Henderson

## Examples

``` r
featMat <- calculate_features(data = simData, 
  feature_set = "catch22")
#> Running computations for catch22...
#> Warning: There was 1 warning in `dplyr::reframe()`.
#> ℹ In argument: `Rcatch22::catch22_all(values, catch24 = catch24)`.
#> ℹ In group 1: `id = "AR(1)_1"` `process = "AR(1)"`.
#> Caused by warning:
#> ! As of 0.1.14 the feature 'CO_f1ecac' returns a double instead of int
#> This warning is displayed once per session.
```
