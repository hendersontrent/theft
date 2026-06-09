# Calculate quantiles combined with FFT absolute value and angle coefficients

Computes 101 quantiles together with the absolute value and angle of FFT
coefficients 0-99, following the same convention as `tsfresh`'s
`fft_coefficient` feature with `attr = "abs"` and `attr = "angle"`.

## Usage

``` r
fftquantiles(y, quantiles = seq(0, 1, by = 0.01))
```

## Arguments

- y:

  `numeric` vector of values

- quantiles:

  `numeric` vector of quantiles to calculate. Defaults to
  `seq(0, 1, by = 0.01)`

## Value

`data.frame` of results

## References

Christ, M., Braun, N., Neuffer, J., and Kempa-Liehr, A.W. (2018). Time
Series FeatuRe Extraction on basis of Scalable Hypothesis tests (tsfresh
– A Python package). *Neurocomputing*, **307**, 72–77.
[doi:10.1016/j.neucom.2018.03.067](https://doi.org/10.1016/j.neucom.2018.03.067)

## Author

Trent Henderson
