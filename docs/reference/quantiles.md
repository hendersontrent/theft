# Calculate a basic set of quantiles for an input time-series vector

By default computes 101 quantiles, from 0 to 1 in steps of 0.01.

## Usage

``` r
quantiles(y, quantiles = seq(0, 1, by = 0.01))
```

## Arguments

- y:

  `numeric` vector of values

- quantiles:

  `numeric` vector of quantiles to calculate. Defaults to
  `seq(0, 1, by = 0.01)`

## Value

`data.frame` of results

## Author

Trent Henderson
