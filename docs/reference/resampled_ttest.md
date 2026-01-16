# Compute correlated t-statistic and p-value for resampled data from correctR package

Compute correlated t-statistic and p-value for resampled data from
correctR package

## Usage

``` r
resampled_ttest(x, y, n, n1, n2)
```

## Arguments

- x:

  `numeric` vector of values for model A

- y:

  `numeric` vector of values for model B

- n:

  `integer` denoting number of repeat samples. Defaults to `length(x)`

- n1:

  `integer` denoting train set size

- n2:

  `integer` denoting test set size

## Value

object of class `data.frame`

## References

Nadeau, C., and Bengio, Y. Inference for the Generalization Error.
Machine Learning 52, (2003).

Bouckaert, R. R., and Frank, E. Evaluating the Replicability of
Significance Tests for Comparing Learning Algorithms. Advances in
Knowledge Discovery and Data Mining. PAKDD 2004. Lecture Notes in
Computer Science, 3056, (2004).

## Author

Trent Henderson
