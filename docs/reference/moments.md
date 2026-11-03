# Calculate a basic set of the four moments of the distribution

Returns the mean, the sample variance (divisor \\n - 1\\), the
moment-ratio skewness \\g_1 = m_3 / m_2^{3/2}\\ and the moment-ratio
kurtosis \\b_2 = m_4 / m_2^2\\ (not excess kurtosis, so a Gaussian
sample gives values near 3), where \\m_k\\ is the \\k\\-th sample
central moment with divisor \\n\\.

## Usage

``` r
moments(y)
```

## Arguments

- y:

  `numeric` vector of values

## Value

`data.frame` of results

## Author

Trent Henderson
