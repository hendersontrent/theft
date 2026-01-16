# Produce a correlation matrix plot showing pairwise correlations of feature vectors by unique id with automatic hierarchical clustering.

Produce a correlation matrix plot showing pairwise correlations of
feature vectors by unique id with automatic hierarchical clustering.

## Usage

``` r
plot_feature_correlations(
  data,
  is_normalised = NULL,
  id_var = "id",
  names_var = "names",
  values_var = "values",
  method = NULL,
  cor_method = c("pearson", "spearman"),
  clust_method = c("average", "ward.D", "ward.D2", "single", "complete", "mcquitty",
    "median", "centroid"),
  interactive = FALSE
)
```

## Arguments

- data:

  a dataframe with at least 3 columns for `'id'`, `'names'` and
  `'values'`

- is_normalised:

  deprecated as of 0.4.0; do not use

- id_var:

  a string specifying the ID variable to compute pairwise correlations
  between. Defaults to `"id"`

- names_var:

  a string denoting the name of the variable/column that holds the
  feature names. Defaults to `"names"`

- values_var:

  a string denoting the name of the variable/column that holds the
  numerical feature values. Defaults to `"values"`

- method:

  deprecated as of 0.4.0; do not use

- cor_method:

  the correlation method to use. Defaults to `"pearson"`

- clust_method:

  the hierarchical clustering method to use for the pairwise correlation
  plot. Defaults to `"average"`

- interactive:

  a Boolean as to whether to plot an interactive `plotly` graphic.
  Defaults to `FALSE`

## Value

an object of class `ggplot` that contains the correlation matrix graphic

## Author

Trent Henderson

## Examples

``` r
featMat <- calculate_features(data = simData, 
  id_var = "id", 
  time_var = "timepoint", 
  values_var = "values", 
  group_var = "process", 
  feature_set = "catch22",
  seed = 123)
#> No IDs removed. All value vectors good for feature extraction.
#> Running computations for catch22...
#> 
#> Calculations completed for catch22.
  
plot_feature_correlations(data = featMat, 
  id_var = "id", 
  names_var = "names", 
  values_var = "values",
  cor_method = "pearson",
  clust_method = "average",
  interactive = FALSE)

```
