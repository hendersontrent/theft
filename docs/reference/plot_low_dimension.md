# Produce a principal components analysis (PCA) on normalised feature values and render a bivariate plot to visualise it

Produce a principal components analysis (PCA) on normalised feature
values and render a bivariate plot to visualise it

## Usage

``` r
plot_low_dimension(
  data,
  is_normalised = FALSE,
  id_var = "id",
  group_var = NULL,
  method = c("z-score", "Sigmoid", "RobustSigmoid", "MinMax"),
  low_dim_method = c("PCA", "t-SNE"),
  perplexity = 30,
  plot = TRUE,
  show_covariance = FALSE,
  seed = 123
)
```

## Arguments

- data:

  a dataframe with at least 2 columns called `"names"` and `"values"`

- is_normalised:

  a Boolean as to whether the input feature values have already been
  scaled. Defaults to `FALSE`

- id_var:

  a string specifying the ID variable to uniquely identify each time
  series. Defaults to `"id"`

- group_var:

  a string specifying the grouping variable that the data aggregates to
  (if one exists). Defaults to `NULL`

- method:

  a rescaling/normalising method to apply. Defaults to `"z-score"`

- low_dim_method:

  the low dimensional embedding method to use. Defaults to `"PCA"`

- perplexity:

  the perplexity hyperparameter to use if t-SNE algorithm is selected.
  Defaults to `30`

- plot:

  a Boolean as to whether a plot or model fit information should be
  returned. Defaults to `TRUE`

- show_covariance:

  a Boolean as to whether covariance ellipses should be shown on the
  plot. Defaults to `FALSE`

- seed:

  fixed number for R's random number generator to ensure reproducibility

## Value

if `plot = TRUE`, returns an object of class `ggplot`, if `plot = FALSE`
returns an object of class dataframe with PCA results

## Author

Trent Henderson

## Examples

``` r
# \donttest{
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

plot_low_dimension(featMat, 
  is_normalised = FALSE, 
  id_var = "id", 
  group_var = "group", 
  method = "RobustSigmoid", 
  low_dim_method = "PCA", 
  plot = TRUE,
  show_covariance = TRUE,
  seed = 123)

# }
```
