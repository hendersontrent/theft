# Return an object containing results from top-performing features on a classification task

Return an object containing results from top-performing features on a
classification task

## Usage

``` r
compute_top_features(
  data,
  num_features = 40,
  normalise_violin_plots = FALSE,
  method = c("z-score", "Sigmoid", "RobustSigmoid", "MinMax"),
  cor_method = c("pearson", "spearman"),
  test_method = "gaussprRadial",
  clust_method = c("average", "ward.D", "ward.D2", "single", "complete", "mcquitty",
    "median", "centroid"),
  use_balanced_accuracy = FALSE,
  use_k_fold = FALSE,
  num_folds = 10,
  use_empirical_null = FALSE,
  null_testing_method = c("ModelFreeShuffles", "NullModelFits"),
  p_value_method = c("empirical", "gaussian"),
  num_permutations = 50,
  pool_empirical_null = FALSE,
  seed = 123
)
```

## Arguments

- data:

  the `feature_calculations` object containing the raw feature matrix
  produced by `calculate_features`

- num_features:

  `integer` denoting the number of top features to retain and explore.
  Defaults to `40`

- normalise_violin_plots:

  `Boolean` of whether to normalise features before plotting. Defaults
  to `FALSE`

- method:

  a rescaling/normalising method to apply to violin plots. Defaults to
  `"z-score"`

- cor_method:

  `string` denoting the correlation method to use. Defaults to
  `"pearson"`

- test_method:

  `string` specifying the algorithm to use for quantifying class
  separation. Defaults to `"gaussprRadial"`. Should be either
  `"t-test"`, `"wilcox"`, or `"binomial logistic"` for two-class
  problems to obtain exact statistics, or a valid `caret` classification
  model for everything else

- clust_method:

  `string` denoting the hierarchical clustering method to use for the
  pairwise correlation plot. Defaults to `"average"`

- use_balanced_accuracy:

  `Boolean` specifying whether to use balanced accuracy as the summary
  metric for caret model training. Defaults to `FALSE`

- use_k_fold:

  `Boolean` specifying whether to use k-fold procedures for generating a
  distribution of classification accuracy estimates if a `caret` model
  is specified for `test_method`. Defaults to ` FALSE`

- num_folds:

  `integer` specifying the number of k-folds to perform if `use_k_fold`
  is set to `TRUE`. Defaults to `10`

- use_empirical_null:

  `Boolean` specifying whether to use empirical null procedures to
  compute p-values if a `caret` model is specified for `test_method`.
  Defaults to `FALSE`

- null_testing_method:

  `string` specifying the type of statistical method to use to calculate
  p-values. Defaults to `"ModelFreeShuffles"`

- p_value_method:

  `string` specifying the method of calculating p-values. Defaults to
  `"empirical"`

- num_permutations:

  `integer` specifying the number of class label shuffles to perform if
  `use_empirical_null` is `TRUE`. Defaults to `50`

- pool_empirical_null:

  `Boolean` specifying whether to use the pooled empirical null
  distribution of all features or each features' individual empirical
  null distribution if a `caret` model is specified for `test_method`
  use_empirical_null is `TRUE`. Defaults to `FALSE`

- seed:

  `integer` denoting a fixed number for R's random number generator to
  ensure reproducibility

## Value

an object of class `list` containing a `data.frame` of results, a
`ggplot` feature x feature matrix plot, and a `ggplot` violin plot

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
  
compute_top_features(featMat,
  num_features = 10,
  normalise_violin_plots = FALSE,
  method = "RobustSigmoid",
  cor_method = "pearson",
  test_method = "gaussprRadial",
  clust_method = "average",
  use_balanced_accuracy = FALSE,
  use_k_fold = FALSE,
  num_folds = 10,
  use_empirical_null = TRUE,
  null_testing_method = "ModelFreeShuffles",
  p_value_method = "gaussian",
  num_permutations = 100,
  pool_empirical_null = FALSE,
  seed = 123) 
#> This will take a while. Great reason to go grab a coffee and relax ^_^
#> 
#> Selecting top features using p-value.
#> $ResultsTable
#>                                         feature  accuracy p_value_accuracy
#> 1                             catch22_co_f1ecac 0.7666667     1.010919e-96
#> 2      catch22_sp_summaries_welch_rect_area_5_1 0.7055556     1.988011e-78
#> 3          catch22_fc_local_simple_mean3_stderr 0.7055556     1.988011e-78
#> 4      catch22_sp_summaries_welch_rect_centroid 0.6833333     2.923146e-72
#> 5                       catch22_co_first_min_ac 0.6222222     1.240886e-56
#> 6     catch22_sb_binary_stats_mean_longstretch1 0.6166667     2.615618e-55
#> 7                         catch22_co_trev_1_num 0.6055556     1.039544e-52
#> 8            catch22_sb_motif_three_quantile_hh 0.6055556     1.039544e-52
#> 9  catch22_co_embed2_dist_tau_d_expfit_meandiff 0.5611111     5.869639e-43
#> 10            catch22_co_histogram_ami_even_2_5 0.5555556     8.222765e-42
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
#> 
#> $FeatureFeatureCorrelationPlot

#> 
#> $ViolinPlots

#> 
# }
```
