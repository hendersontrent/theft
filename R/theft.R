#' 
#' @docType package
#' @aliases theft-package
#' @name theft
#' @title Tools for Handling Extraction of Features from Time-series
#' 
#' @description Tools for Handling Extraction of Features from Time-series
#' 
#' @importFrom rlang .data as_function is_bool abort warn sym
#' @importFrom stats median cor cor.test dist hclust prcomp reorder sd var p.adjust quantile qt
#' @import tibble
#' @importFrom broom augment tidy
#' @importFrom R.matlab readMat
#' @import dplyr
#' @import ggplot2
#' @importFrom e1071 svm
#' @importFrom purrr map map_dfr
#' @importFrom janitor clean_names
#' @importFrom correctR resampled_ttest
#' @importFrom normaliseR normalise
#' @importFrom purloiner extract_features
#' @importFrom pilfer reduce_dimensions
NULL
