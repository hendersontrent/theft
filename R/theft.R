#' 
#' @docType package
#' @aliases theft-package
#' @name theft
#' @title Tools for Handling Extraction of Features from Time-series
#' 
#' @description Tools for Handling Extraction of Features from Time-series
#' 
#' @importFrom rlang .data as_function is_bool abort warn sym
#' @importFrom stats IQR cor cor.test dist hclust median prcomp reorder sd var p.adjust quantile qt na.omit
#' @import Rcatch22
#' @importFrom tsfeatures lumpiness stability max_level_shift max_var_shift max_kl_shift crossing_points flat_spots hurst compengine autocorr_features pred_features station_features dist_features scal_features embed2_incircle firstzero_ac ac_9 firstmin_ac trev_num motiftwo_entro3 binarize_mean walker_propcross localsimple_taures sampen_first sampenc std1st_der spreadrandomlocal_meantaul histogram_mode outlierinclude_mdrmd fluctanal_prop_r1 entropy tsfeatures stl_features acf_features pacf_features holt_parameters hw_parameters heterogeneity nonlinearity arch_stat
#' @import feasts
#' @import tsibble
#' @importFrom scales rescale
#' @importFrom tidyr gather unnest_wider pivot_longer pivot_wider drop_na crossing
#' @importFrom fabletools features feature_set
#' @import tibble
#' @importFrom broom augment tidy
#' @importFrom R.matlab readMat
#' @import reticulate
#' @import dplyr
#' @import ggplot2
#' @importFrom e1071 svm
#' @importFrom purrr map map_dfr
#' @importFrom janitor clean_names
NULL
