# Fit a classifier to feature matrix using all features or all features by set

Fit a classifier to feature matrix using all features or all features by
set

## Usage

``` r
fit_multi_feature_classifier(
  data,
  by_set = FALSE,
  test_method = "gaussprRadial",
  use_balanced_accuracy = FALSE,
  use_k_fold = TRUE,
  num_folds = 10,
  use_empirical_null = FALSE,
  null_testing_method = c("ModelFreeShuffles", "NullModelFits"),
  p_value_method = c("empirical", "gaussian"),
  num_permutations = 100,
  seed = 123
)
```

## Arguments

- data:

  the `feature_calculations` object containing the raw feature matrix
  produced by `calculate_features`

- by_set:

  `Boolean` specifying whether to compute classifiers for each feature
  set. Defaults to `FALSE`

- test_method:

  `character` specifying the algorithm to use for quantifying class
  separation. Defaults to `"gaussprRadial"`. Must be a valid `caret`
  classification model

- use_balanced_accuracy:

  `Boolean` specifying whether to use balanced accuracy as the summary
  metric for caret model training. Defaults to `FALSE`

- use_k_fold:

  `Boolean` specifying whether to use k-fold procedures for generating a
  distribution of classification accuracy estimates. Defaults to `TRUE`

- num_folds:

  `integer` specifying the number of folds (train-test splits) to
  perform if `use_k_fold` is set to `TRUE`. Defaults to `10`

- use_empirical_null:

  `Boolean` specifying whether to use empirical null procedures to
  compute p-values. Defaults to `FALSE`

- null_testing_method:

  `character` specifying the type of statistical method to use to
  calculate p-values. Defaults to `model free shuffles`

- p_value_method:

  `character` specifying the method of calculating p-values. Defaults to
  `"empirical"`

- num_permutations:

  `integer` specifying the number of class label shuffles to perform if
  `use_empirical_null` is `TRUE`. Defaults to `100`

- seed:

  `integer` to fix R's random number generator to ensure
  reproducibility. Defaults to `123`

## Value

an object of class `list` containing a `data.frame` summary of raw
classification results, a `data.frame` summary of the test statistics,
and a `ggplot` object if `by_set` is `TRUE`

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

fit_multi_feature_classifier(featMat,
  by_set = FALSE,
  test_method = "gaussprRadial",
  use_balanced_accuracy = FALSE,
  use_k_fold = TRUE,
  num_folds = 10,
  use_empirical_null = TRUE,
  null_testing_method = "ModelFreeShuffles",
  p_value_method = "gaussian",
  num_permutations = 50,
  seed = 123)
#> Assessing feature values and unique IDs for NAs using matrix of all features.
#> Loading required package: ggplot2
#> Loading required package: lattice
#> $TestStatistics
#>    accuracy p_value_accuracy classifier_name               statistic_name
#> 1 0.6722222     7.518342e-61   gaussprRadial Mean classification accuracy
#> 
#> $RawClassificationResults
#>     accuracy accuracy_sd category            method num_features_used
#> 1  0.6722222  0.08050765     Main              <NA>                NA
#> 2  0.1222222          NA     Null ModelFreeShuffles                NA
#> 3  0.2277778          NA     Null ModelFreeShuffles                NA
#> 4  0.1444444          NA     Null ModelFreeShuffles                NA
#> 5  0.1444444          NA     Null ModelFreeShuffles                NA
#> 6  0.1444444          NA     Null ModelFreeShuffles                NA
#> 7  0.1777778          NA     Null ModelFreeShuffles                NA
#> 8  0.1888889          NA     Null ModelFreeShuffles                NA
#> 9  0.1777778          NA     Null ModelFreeShuffles                NA
#> 10 0.1611111          NA     Null ModelFreeShuffles                NA
#> 11 0.1277778          NA     Null ModelFreeShuffles                NA
#> 12 0.1722222          NA     Null ModelFreeShuffles                NA
#> 13 0.1611111          NA     Null ModelFreeShuffles                NA
#> 14 0.1555556          NA     Null ModelFreeShuffles                NA
#> 15 0.1555556          NA     Null ModelFreeShuffles                NA
#> 16 0.2055556          NA     Null ModelFreeShuffles                NA
#> 17 0.1944444          NA     Null ModelFreeShuffles                NA
#> 18 0.1500000          NA     Null ModelFreeShuffles                NA
#> 19 0.1777778          NA     Null ModelFreeShuffles                NA
#> 20 0.1444444          NA     Null ModelFreeShuffles                NA
#> 21 0.1500000          NA     Null ModelFreeShuffles                NA
#> 22 0.1611111          NA     Null ModelFreeShuffles                NA
#> 23 0.1277778          NA     Null ModelFreeShuffles                NA
#> 24 0.1722222          NA     Null ModelFreeShuffles                NA
#> 25 0.2333333          NA     Null ModelFreeShuffles                NA
#> 26 0.2222222          NA     Null ModelFreeShuffles                NA
#> 27 0.1777778          NA     Null ModelFreeShuffles                NA
#> 28 0.2222222          NA     Null ModelFreeShuffles                NA
#> 29 0.1611111          NA     Null ModelFreeShuffles                NA
#> 30 0.2222222          NA     Null ModelFreeShuffles                NA
#> 31 0.2111111          NA     Null ModelFreeShuffles                NA
#> 32 0.1388889          NA     Null ModelFreeShuffles                NA
#> 33 0.1388889          NA     Null ModelFreeShuffles                NA
#> 34 0.1666667          NA     Null ModelFreeShuffles                NA
#> 35 0.1833333          NA     Null ModelFreeShuffles                NA
#> 36 0.1444444          NA     Null ModelFreeShuffles                NA
#> 37 0.1500000          NA     Null ModelFreeShuffles                NA
#> 38 0.1722222          NA     Null ModelFreeShuffles                NA
#> 39 0.1888889          NA     Null ModelFreeShuffles                NA
#> 40 0.1888889          NA     Null ModelFreeShuffles                NA
#> 41 0.1722222          NA     Null ModelFreeShuffles                NA
#> 42 0.1555556          NA     Null ModelFreeShuffles                NA
#> 43 0.1555556          NA     Null ModelFreeShuffles                NA
#> 44 0.1944444          NA     Null ModelFreeShuffles                NA
#> 45 0.1555556          NA     Null ModelFreeShuffles                NA
#> 46 0.1277778          NA     Null ModelFreeShuffles                NA
#> 47 0.1444444          NA     Null ModelFreeShuffles                NA
#> 48 0.1777778          NA     Null ModelFreeShuffles                NA
#> 49 0.1833333          NA     Null ModelFreeShuffles                NA
#> 50 0.2000000          NA     Null ModelFreeShuffles                NA
#> 51 0.2555556          NA     Null ModelFreeShuffles                NA
#>    classifier_name               statistic_name
#> 1    gaussprRadial Mean classification accuracy
#> 2    gaussprRadial Mean classification accuracy
#> 3    gaussprRadial Mean classification accuracy
#> 4    gaussprRadial Mean classification accuracy
#> 5    gaussprRadial Mean classification accuracy
#> 6    gaussprRadial Mean classification accuracy
#> 7    gaussprRadial Mean classification accuracy
#> 8    gaussprRadial Mean classification accuracy
#> 9    gaussprRadial Mean classification accuracy
#> 10   gaussprRadial Mean classification accuracy
#> 11   gaussprRadial Mean classification accuracy
#> 12   gaussprRadial Mean classification accuracy
#> 13   gaussprRadial Mean classification accuracy
#> 14   gaussprRadial Mean classification accuracy
#> 15   gaussprRadial Mean classification accuracy
#> 16   gaussprRadial Mean classification accuracy
#> 17   gaussprRadial Mean classification accuracy
#> 18   gaussprRadial Mean classification accuracy
#> 19   gaussprRadial Mean classification accuracy
#> 20   gaussprRadial Mean classification accuracy
#> 21   gaussprRadial Mean classification accuracy
#> 22   gaussprRadial Mean classification accuracy
#> 23   gaussprRadial Mean classification accuracy
#> 24   gaussprRadial Mean classification accuracy
#> 25   gaussprRadial Mean classification accuracy
#> 26   gaussprRadial Mean classification accuracy
#> 27   gaussprRadial Mean classification accuracy
#> 28   gaussprRadial Mean classification accuracy
#> 29   gaussprRadial Mean classification accuracy
#> 30   gaussprRadial Mean classification accuracy
#> 31   gaussprRadial Mean classification accuracy
#> 32   gaussprRadial Mean classification accuracy
#> 33   gaussprRadial Mean classification accuracy
#> 34   gaussprRadial Mean classification accuracy
#> 35   gaussprRadial Mean classification accuracy
#> 36   gaussprRadial Mean classification accuracy
#> 37   gaussprRadial Mean classification accuracy
#> 38   gaussprRadial Mean classification accuracy
#> 39   gaussprRadial Mean classification accuracy
#> 40   gaussprRadial Mean classification accuracy
#> 41   gaussprRadial Mean classification accuracy
#> 42   gaussprRadial Mean classification accuracy
#> 43   gaussprRadial Mean classification accuracy
#> 44   gaussprRadial Mean classification accuracy
#> 45   gaussprRadial Mean classification accuracy
#> 46   gaussprRadial Mean classification accuracy
#> 47   gaussprRadial Mean classification accuracy
#> 48   gaussprRadial Mean classification accuracy
#> 49   gaussprRadial Mean classification accuracy
#> 50   gaussprRadial Mean classification accuracy
#> 51   gaussprRadial Mean classification accuracy
#> 
# }
```
