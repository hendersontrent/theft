# Project a feature matrix into a low dimensional representation using PCA or t-SNE

Project a feature matrix into a low dimensional representation using PCA
or t-SNE

## Usage

``` r
reduce_dims(
  data,
  norm_method = c("zScore", "Sigmoid", "RobustSigmoid", "MinMax"),
  unit_int = FALSE,
  low_dim_method = c("PCA", "tSNE"),
  na_removal = c("feature", "sample"),
  perplexity = 10,
  seed = 123,
  ...
)
```

## Arguments

- data:

  the `feature_calculations` object containing the raw feature matrix
  produced by `calculate_features`

- norm_method:

  `character` denoting the rescaling/normalising method to apply. Can be
  one of `"z-score"`, `"Sigmoid"`, `"RobustSigmoid"`, or `"MinMax"`.
  Defaults to `"z-score"`

- unit_int:

  `Boolean` whether to rescale into unit interval `[0,1]` after applying
  normalisation method. Defaults to `FALSE`

- low_dim_method:

  `character` specifying the low dimensional embedding method to use.
  Can be one of `"PCA"` or `"tSNE"`. Defaults to `"PCA"`

- na_removal:

  `character` defining the way to deal with NAs produced during feature
  calculation. Can be one of `"feature"` or `"sample"`. `"feature"`
  removes all features that produced any NAs in any sample, keeping the
  number of samples the same. `"sample"` omits all samples that produced
  at least one NA. Defaults to `"feature"`

- perplexity:

  `integer` denoting the perplexity hyperparameter to use if
  `low_dim_method` is `"t-SNE"`. Defaults to `10`

- seed:

  `integer` to fix R's random number generator to ensure
  reproducibility. Defaults to `123`

- ...:

  arguments to be passed to either
  [`stats::prcomp`](https://rdrr.io/r/stats/prcomp.html) or
  [`Rtsne::Rtsne`](https://rdrr.io/pkg/Rtsne/man/Rtsne.html) depending
  on whether `"low_dim_method"` is `"PCA"` or `"t-SNE"`

## Value

object of class `low_dimension`

## Author

Trent Henderson
