# Scale each feature vector into a user-specified range for visualisation and modelling

Scale each feature vector into a user-specified range for visualisation
and modelling

## Usage

``` r
normalize_feature_frame(
  data,
  names_var = "names",
  values_var = "values",
  method = c("z-score", "Sigmoid", "RobustSigmoid", "MinMax")
)
```

## Arguments

- data:

  a dataframe with at least 2 columns: names variable (feature names)
  and value variable

- names_var:

  a string denoting the name of the variable/column that holds the
  feature names. Defaults to `"names"`

- values_var:

  a string denoting the name of the variable/column that holds the
  numerical feature values. Defaults to `"values"`

- method:

  a rescaling/normalising method to apply. Defaults to `"RobustSigmoid"`

## Value

a dataframe with the value column rescaled into the specified range

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
  
normed <- normalize_feature_frame(featMat, 
  names_var = "names", 
  values_var = "values", 
  method = "RobustSigmoid")
```
