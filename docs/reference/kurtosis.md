# Calculate the kurtosis of a vector

Moment-ratio kurtosis \\b_2 = m_4 / m_2^2\\, where \\m_k\\ is the
\\k\\-th sample central moment (with divisor \\n\\). This is not excess
kurtosis: a Gaussian sample gives values near 3.

## Usage

``` r
kurtosis(y)
```

## Arguments

- y:

  `numeric` vector of values

## Value

`numeric` scalar of kurtosis

## Author

Trent Henderson
