# Produce a correlation matrix plot showing pairwise correlations of time series with automatic hierarchical clustering

Produce a correlation matrix plot showing pairwise correlations of time
series with automatic hierarchical clustering

## Usage

``` r
plot_ts_correlations(
  data,
  is_normalised = NULL,
  id_var = "id",
  time_var = "timepoint",
  values_var = "values",
  method = NULL,
  clust_method = c("average", "ward.D", "ward.D2", "single", "complete", "mcquitty",
    "median", "centroid"),
  cor_method = c("pearson", "spearman"),
  interactive = FALSE
)
```

## Arguments

- data:

  a dataframewith at least 2 columns for `"id"` and `"values"` variables

- is_normalised:

  deprecated as of 0.4.0; do not use

- id_var:

  a string specifying the ID variable to compute pairwise correlations
  between. Defaults to `"id"`

- time_var:

  a string specifying the time index variable. Defaults to `NULL`

- values_var:

  a string denoting the name of the variable/column that holds the
  numerical feature values. Defaults to `"values"`

- method:

  deprecated as of 0.4.0; do not use

- clust_method:

  the hierarchical clustering method to use for the pairwise correlation
  plot. Defaults to `"average"`

- cor_method:

  the correlation method to use. Defaults to `"pearson"`

- interactive:

  a Boolean as to whether to plot an interactive `plotly` graphic.
  Defaults to `FALSE`

## Value

an object of class `ggplot`

## Author

Trent Henderson

## Examples

``` r
plot_ts_correlations(data = simData, 
  id_var = "id", 
  time_var = "timepoint",
  values_var = "values",
  method = "RobustSigmoid",
  cor_method = "pearson",
  clust_method = "average",
  interactive = FALSE)
#> Warning: As of 0.4.0 'is_normalised' and 'method' are no longer arguments to plot_ts_correlations
#> This warning is displayed once per session.

```
