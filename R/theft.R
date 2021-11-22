#' 
#' @docType package
#' @name theft
#' @title Tools for Handling Extraction of Features from Time-series
#' 
#' @description Tools for Handling Extraction of Features from Time-series
#' 
#' @importFrom stats IQR
#' @importFrom stats cor
#' @importFrom stats dist
#' @importFrom stats hclust
#' @importFrom stats median
#' @importFrom stats prcomp
#' @importFrom stats reorder
#' @importFrom stats sd
#' @importFrom magrittr %>%
#' @import Rcatch22
#' @importFrom tsfeatures lumpiness stability max_level_shift max_var_shift max_kl_shift crossing_points flat_spots hurst compengine autocorr_features pred_features station_features dist_features scal_features embed2_incircle firstzero_ac ac_9 firstmin_ac trev_num motiftwo_entro3 binarize_mean walker_propcross localsimple_taures sampen_first sampenc std1st_der spreadrandomlocal_meantaul histogram_mode outlierinclude_mdrmd fluctanal_prop_r1 entropy tsfeatures stl_features acf_features pacf_features holt_parameters hw_parameters heterogeneity nonlinearity arch_stat
#' @import feasts
#' @import tsibble
#' @importFrom scales rescale
#' @importFrom tidyr gather
#' @importFrom tidyr unnest_wider
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr drop_na
#' @importFrom data.table rbindlist
#' @importFrom fabletools features
#' @importFrom fabletools feature_set
#' @import tibble
#' @importFrom broom augment
#' @importFrom broom tidy
#' @importFrom R.matlab readMat
#' @import reticulate
#' @import dplyr
#' @importFrom plotly ggplotly
#' @importFrom plotly config
#' @importFrom RColorBrewer brewer.pal
NULL
