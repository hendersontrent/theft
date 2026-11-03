# Calculate quantiles combined with relative spectral power in fixed frequency bands

Computes 101 quantiles (0 to 1 in steps of 0.01, by default) together
with the 100 relative band powers of
[`fft_features`](https://hendersontrent.github.io/theft/reference/fft_features.md)
(and, with `phase = TRUE`, their phases).

## Usage

``` r
fftquantiles(y, quantiles = seq(0, 1, by = 0.01), squared, phase = FALSE)
```

## Arguments

- y:

  `numeric` vector of values

- quantiles:

  `numeric` vector of quantiles to calculate. Defaults to
  `seq(0, 1, by = 0.01)`

- squared:

  Deprecated and ignored: the spectral features are relative power, so
  there is no magnitude/squared-magnitude choice

- phase:

  `Boolean` specifying whether to also return each band's phase as its
  cosine and sine (see
  [`fft_features`](https://hendersontrent.github.io/theft/reference/fft_features.md)).
  Only meaningful when the series share a time origin. Defaults to
  `FALSE`

## Value

`data.frame` of results

## Author

Trent Henderson
