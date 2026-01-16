# Helper function to create a resampled dataset

Helper function to create a resampled dataset

## Usage

``` r
resample_data(data, train_rows, test_rows, train_groups, test_groups, seed)
```

## Arguments

- data:

  `data.frame` containing time-series features

- train_rows:

  `integer` denoting the number of cases in the train set

- test_rows:

  `integer` denoting the number of cases in the test set

- train_groups:

  `data.frame` containing proportions of each class in original train
  split

- test_groups:

  `data.frame` containing proportions of each class in original test
  split

- seed:

  `integer` denoting fixed value for R's pseudorandom number generator

## Value

`list` containing new train and test data

## Author

Trent Henderson
