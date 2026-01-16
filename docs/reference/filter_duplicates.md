# Remove duplicate features that exist in multiple feature sets and retain a reproducible random selection of one of them

Remove duplicate features that exist in multiple feature sets and retain
a reproducible random selection of one of them

## Usage

``` r
filter_duplicates(data, preference = NULL, seed = 123)
```

## Arguments

- data:

  `feature_calculations` object containing the raw feature matrix
  produced by `calculate_features`

- preference:

  deprecated. Do not use

- seed:

  `integer` denoting a fix for R's pseudo-random number generator to
  ensure selections are reproducible. Defaults to `123`

## Value

`feature_calculations` object containing filtered feature data

## Author

Trent Henderson
