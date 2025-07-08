
"_PACKAGE"
#' @name theft
#' @title Tools for Handling Extraction of Features from Time-series
#' 
#' @description Tools for Handling Extraction of Features from Time-series
#' 
#' @importFrom utils packageVersion
#' @importFrom rlang .data
#' @importFrom stats ts
#' @importFrom dplyr group_by filter ungroup bind_rows across all_of mutate arrange reframe mutate select distinct row_number left_join everything rename inner_join sym
#' @importFrom tsibble key_vars index_var as_tsibble
#' @importFrom fabletools features feature_set
#' @importFrom Rcatch22 catch22_all
#' @importFrom tsfeatures tsfeatures
#' @importFrom tidyr gather unnest_wider pivot_longer
#' @importFrom fabletools features feature_set
#' @importFrom R.matlab readMat
#' @importFrom reticulate virtualenv_create virtualenv_install use_virtualenv import source_python
#' @importFrom purrr map_df
#' @import feasts
NULL
