#------------------- Helper functions to reduce length -------------

# catch22

calc_catch22 <- function(data){
  
  outData <- data %>%
    tibble::as_tibble() %>%
    dplyr::group_by(id) %>%
    dplyr::arrange(timepoint) %>%
    dplyr::summarise(names = Rcatch22::catch22_all(values)$names,
                     values = Rcatch22::catch22_all(values)$values) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(method = "catch22")
  
  return(outData)
}

# feasts

calc_feasts <- function(data){
  
  tsData <- tsibble::as_tsibble(data, key = id, index = timepoint)
  
  outData <- tsData %>%
    fabletools::features(values, fabletools::feature_set(pkgs = "feasts")) %>%
    tidyr::pivot_longer(!id, names_to = "names", values_to = "values") %>%
    dplyr::mutate(method = "feasts")
  
  return(outData)
}

# tsfeatures

calc_tsfeatures <- function(data){
  
  outData <- data %>%
    tibble::as_tibble() %>%
    dplyr::group_by(id) %>%
    dplyr::arrange(timepoint) %>%
    dplyr::select(-c(timepoint)) %>%
    dplyr::summarise(values = list(values)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(tsfeatures::tsfeatures(values, features = c("frequency", "stl_features", "entropy", "acf_features",
                                                                 "compengine", "arch_stat", "crossing_points", "flat_spots",
                                                                 "heterogeneity", "holt_parameters", "hurst", 
                                                                 "lumpiness", "max_kl_shift", "max_level_shift", "max_var_shift", 
                                                                 "nonlinearity", "pacf_features", "stability", "unitroot_kpss",
                                                                 "unitroot_pp", "embed2_incircle", "firstzero_ac",
                                                                 "histogram_mode", "localsimple_taures", "sampenc",
                                                                 "spreadrandomlocal_meantaul"))) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(!id, names_to = "names", values_to = "values") %>%
    dplyr::mutate(method = "tsfeatures")
  
  return(outData)
}

#------------------- Main exported calculation function ------------

#' Automatically run time-series feature calculations included in the package
#' @import dplyr
#' @importFrom magrittr %>%
#' @import Rcatch22
#' @import feasts
#' @import tsfeatures
#' @import tsibble
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer
#' @importFrom data.table rbindlist
#' @importFrom fabletools features
#' @importFrom fabletools feature_set
#' @param data a dataframe with at least 4 columns: id variable, group variable, time variable, value variable
#' @param id_var a string specifying the ID variable to group data on (if one exists). Defaults to NULL
#' @param time_var a string specifying the time index variable. Defaults to NULL
#' @param feature_set The set of time-series features to calculate. Defaults to 'all'
#' @return object of class DataFrame that contains the summary statistics for each feature
#' @author Trent Henderson
#' @export
#' @examples
#' \dontrun{
#' library(dplyr)
#' d <- tsibbledata::aus_retail %>%
#'   filter(State == "New South Wales")
#' outs <- calculate_features(data = d, id_var = "Industry", time_var = "Month", values_var = "Turnover", feature_set = "all")
#' }
#'

calculate_features <- function(data, id_var = NULL, time_var = NULL, values_var = NULL,
                               feature_set = c("all", "catch22", "feasts", "tsfeatures")){
  
  if(is.null(id_var) || is.null(time_var) || is.null(values_var)){
    stop("As {tsibble} currently cannot handle numeric vectors, input must be a dataframe with at least 3 columns: id, timepoint, value")
  }
  
  # Make 'all' the default
  
  if(missing(feature_set)){
    feature_set <- "all"
  }
  
  if(is.null(feature_set)){
    feature_set <- "all"
  }
  
  #--------- Error catches ---------
  
  # Method selection
  
  the_sets <- c("all", "catch22", "feasts", "tsfeatures")
  '%ni%' <- Negate('%in%')
  
  if(feature_set %ni% the_sets){
    stop("feature_set should be a selection or combination of 'all', 'catch22', 'feasts' or 'tsfeatures' entered as a single string or vector for multiple.")
  }
  
  #--------- Feature calcs --------
  
  data_re <- data %>%
    dplyr::rename(id = dplyr::all_of(id_var),
                  timepoint = dplyr::all_of(time_var),
                  values = dplyr::all_of(values_var))
  
  if("all" %in% feature_set){
    
    tmp <- calc_catch22(data = data_re)
    tmp1 <- calc_feasts(data = data_re)
    tmp2 <- calc_tsfeatures(data = data_re)
    
    tmp_all <- dplyr::bind_rows(tmp, tmp1, tmp2)
  }
  
  if("catch22" %in% feature_set){
    
    tmp <- calc_catch22(data = data_re)
  }
  
  if("feasts" %in% feature_set){
    
    tmp1 <- calc_feasts(data = data_re)
  }
  
  if("tsfeatures" %in% feature_set){
    
    tmp2 <- calc_tsfeatures(data = data_re)
  }
  
  tmp_all <- data.frame()
  
  if(exists("tmp")){
    tmp_all <- dplyr::bind_rows(tmp_all, tmp)
  }
  
  if(exists("tmp1")){
    tmp_all <- dplyr::bind_rows(tmp_all, tmp1)
  }
  
  if(exists("tmp2")){
    tmp_all <- dplyr::bind_rows(tmp_all, tmp2)
  }
  
  return(tmp_all)
}
