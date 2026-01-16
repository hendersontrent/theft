# Scale each value into a user-specified range for visualisation and analysis

Scale each value into a user-specified range for visualisation and
analysis

## Usage

``` r
normalize_feature_vector(
  x,
  method = c("z-score", "Sigmoid", "RobustSigmoid", "MinMax")
)
```

## Arguments

- x:

  a vector of scalar values

- method:

  a rescaling/normalising method to apply. Defaults to `"RobustSigmoid"`

## Value

a vector of scalar values normalised into the selected range

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
  
x <- featMat[featMat$names == "DN_HistogramMode_5", ]
xnormed <- normalise_feature_vector(x$values, method = "RobustSigmoid")
```
