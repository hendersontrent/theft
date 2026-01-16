# Produce a heatmap matrix of the calculated feature value vectors and each unique time series with automatic hierarchical clustering.

Produce a heatmap matrix of the calculated feature value vectors and
each unique time series with automatic hierarchical clustering.

## Usage

``` r
plot_all_features(
  data,
  is_normalised = FALSE,
  id_var = "id",
  method = c("z-score", "Sigmoid", "RobustSigmoid", "MinMax"),
  clust_method = c("average", "ward.D", "ward.D2", "single", "complete", "mcquitty",
    "median", "centroid"),
  interactive = FALSE
)
```

## Arguments

- data:

  a dataframe with at least 2 columns called `"names"` and `"values"`

- is_normalised:

  a Boolean as to whether the input feature values have already been
  scaled. Defaults to `FALSE`

- id_var:

  a string specifying the ID variable to identify each time series.
  Defaults to `"id"`

- method:

  a rescaling/normalising method to apply. Defaults to `"RobustSigmoid"`

- clust_method:

  the hierarchical clustering method to use for the pairwise correlation
  plot. Defaults to `"average"`

- interactive:

  a Boolean as to whether to plot an interactive `plotly` graphic.
  Defaults to `FALSE`

## Value

an object of class `ggplot` that contains the heatmap graphic

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

plot_all_features(featMat, 
  is_normalised = FALSE,
  id_var = "id", 
  method = "RobustSigmoid",
  clust_method = "average",
  interactive = FALSE)
#> Applying linear rescaling of values to make plot legend cleaner.

```
