# Produce a plot for a low_dimension object

Produce a plot for a low_dimension object

## Usage

``` r
# S3 method for low_dimension
plot(x, show_covariance = TRUE, ...)
```

## Arguments

- x:

  `low_dimension` object containing the dimensionality reduction
  projection calculated by `reduce_dims`

- show_covariance:

  `Boolean` of whether covariance ellipses should be shown on the plot.
  Defaults to `TRUE`

- ...:

  Arguments to be passed to methods

## Value

object of class `ggplot` that contains the graphic

## Author

Trent Henderson
