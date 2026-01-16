# Fit classification model and compute key metrics

Fit classification model and compute key metrics

## Usage

``` r
fit_models(data, iter_data, row_id, is_null_run = FALSE, classifier)
```

## Arguments

- data:

  `list` containing train and test sets

- iter_data:

  `data.frame` containing the values to iterate over for seed and either
  feature name or set name

- row_id:

  `integer` denoting the row ID for `iter_data` to filter to

- is_null_run:

  `Boolean` whether the calculation is for a null model. Defaults to
  `FALSE`

- classifier:

  `function` specifying the classifier to fit. Should be a function with
  2 arguments: `formula` and `data`. Please note that
  `tsfeature_classifier` z-scores data prior to modelling using the
  train set's information so disabling default scaling if your function
  uses it is recommended.

## Value

`data.frame` of classification results

## Author

Trent Henderson
