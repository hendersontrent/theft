# Calculate FFT absolute value and angle coefficients for an input time-series vector

Computes the absolute value and angle of FFT coefficients 0-99,
following the same convention as `tsfresh`'s `fft_coefficient` feature
with `attr = "abs"` and `attr = "angle"`.

## Usage

``` r
fft_features(y)
```

## Arguments

- y:

  `numeric` vector of values

## Value

`data.frame` of results

## References

Christ, M., Braun, N., Neuffer, J., and Kempa-Liehr, A.W. (2018). Time
Series FeatuRe Extraction on basis of Scalable Hypothesis tests (tsfresh
– A Python package). *Neurocomputing*, **307**, 72–77.
[doi:10.1016/j.neucom.2018.03.067](https://doi.org/10.1016/j.neucom.2018.03.067)

## Author

Trent Henderson
