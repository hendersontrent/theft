# Scale each feature vector into a user-specified range for visualisation and modelling

\`normalise()\` and \`normalize()\` are synonyms.

## Usage

``` r
normalise(
  data,
  norm_method = c("zScore", "Sigmoid", "RobustSigmoid", "MinMax", "MaxAbs"),
  unit_int = FALSE
)

normalize(
  data,
  norm_method = c("zScore", "Sigmoid", "RobustSigmoid", "MinMax", "MaxAbs"),
  unit_int = FALSE
)
```

## Arguments

- data:

  either a `feature_calculations` object containing the raw feature
  matrix produced by `calculate_features` or a `vector` of class
  `numeric` containing values to be rescaled

- norm_method:

  `character` denoting the rescaling/normalising method to apply. Can be
  one of `"zScore"`, `"Sigmoid"`, `"RobustSigmoid"`, `"MinMax"`, or
  `"MaxAbs"`. Defaults to `"zScore"`

- unit_int:

  `Boolean` whether to rescale into unit interval `[0,1]` after applying
  normalisation method. Defaults to `FALSE`

## Value

either an object of class `data.frame` or a `numeric` vector

## Author

Trent Henderson
