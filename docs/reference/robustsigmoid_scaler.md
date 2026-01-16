# Rescales a numeric vector using an outlier-robust Sigmoidal transformation

\\z\_{i} = \left\[1 + \exp\left(-\frac{x\_{i} -
median(\mathbf{x})}{IQR(\mathbf{x})/{1.35}}\right)\right\]^{-1}\\

## Usage

``` r
robustsigmoid_scaler(x)
```

## Arguments

- x:

  `numeric` vector

## Value

`numeric` vector

## References

Fulcher, Ben D., Little, Max A., and Jones, Nick S. Highly Comparative
Time-Series Analysis: The Empirical Structure of Time Series and Their
Methods. Journal of The Royal Society Interface 10(83), (2013).

## Author

Trent Henderson
