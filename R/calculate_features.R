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
    fabletools::features(values, fabletools::feature_set(pkgs = "feasts"))  %>%
    tidyr::gather("names", "values",-id) %>%
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
    tidyr::gather("names", "values",-id) %>%
    dplyr::mutate(method = "tsfeatures")
  
  return(outData)
}

# tsfresh

calc_tsfresh <- function(data, column_id = "id", column_sort = "timepoint", cleanup){
  
  # Load Python function
  
  reticulate::source_python(system.file("python", "tsfresh_calculator.py", package = "theft")) # Ships with package
  
  # Convert time index column to numeric to avoid {tsfresh} errors
  
  if(!is.numeric(data$id) || !is.numeric(data$timepoint)){
    
    ids <- data.frame(old_id = unique(data$id)) %>%
      dplyr::mutate(id = dplyr::row_number())
    
    temp <- data %>%
      dplyr::rename(old_id = id) %>%
      dplyr::left_join(ids, by = c("old_id" = "old_id")) %>%
      dplyr::group_by(id) %>%
      dplyr::arrange(timepoint) %>%
      dplyr::mutate(timepoint = as.numeric(dplyr::row_number())) %>%
      dplyr::ungroup()
    
    # Dropping columns with dplyr::select() isn't working, so just make a new dataframe
    
    temp1 <- data.frame(id = temp$id,
                        timepoint = temp$timepoint,
                        values = temp$values)
    
    # Compute features and re-join back correct id labels
    
    ids2 <- ids %>%
      dplyr::select(-c(id)) %>%
      dplyr::rename(id = old_id)
    
    outData <- tsfresh_calculator(timeseries = temp1, column_id = column_id, column_sort = column_sort, cleanup = cleanup) %>%
      cbind(ids2) %>%
      tidyr::gather("names", "values", -id) %>%
      dplyr::mutate(method = "tsfresh")
    
  } else{
    temp1 <- data.frame(id = data$id,
                        timepoint = data$timepoint,
                        values = data$values)
    
    ids <- unique(temp1$id)
    
    # Do calculations
    
    outData <- tsfresh_calculator(timeseries = temp1, column_id = column_id, column_sort = column_sort, cleanup = cleanup) %>%
      dplyr::mutate(id = ids) %>%
      tidyr::gather("names", "values", -id) %>%
      dplyr::mutate(method = "tsfresh")
  }
  
  return(outData)
}

# TSFEL

calc_tsfel <- function(data){
  
  # Load Python function
  
  reticulate::source_python(system.file("python", "tsfel_calculator.py", package = "theft")) # Ships with package
  
  # Vectorised
  
  outData <- data %>%
    tibble::as_tibble() %>%
    dplyr::group_by(id) %>%
    dplyr::arrange(timepoint) %>%
    dplyr::summarise(tsfel_calculator(values)) %>%
    dplyr::ungroup() %>%
    tidyr::gather("names", "values", -id) %>%
    dplyr::mutate(method = "TSFEL")
  
  return(outData)
}

#------------------- Main exported calculation function ------------

#' Automatically run time-series feature calculations included in the package
#' @import dplyr
#' @importFrom magrittr %>%
#' @import Rcatch22
#' @importFrom tsfeatures lumpiness stability max_level_shift max_var_shift max_kl_shift crossing_points flat_spots hurst compengine autocorr_features pred_features station_features dist_features scal_features embed2_incircle firstzero_ac ac_9 firstmin_ac trev_num motiftwo_entro3 binarize_mean walker_propcross localsimple_taures sampen_first sampenc std1st_der spreadrandomlocal_meantaul histogram_mode outlierinclude_mdrmd fluctanal_prop_r1 entropy tsfeatures stl_features acf_features pacf_features holt_parameters hw_parameters heterogeneity nonlinearity arch_stat
#' @import feasts
#' @import tsibble
#' @import reticulate
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer
#' @importFrom data.table rbindlist
#' @importFrom fabletools features
#' @importFrom fabletools feature_set
#' @param data a dataframe with at least 4 columns: id variable, group variable, time variable, value variable
#' @param id_var a string specifying the ID variable to group data on (if one exists). Defaults to NULL
#' @param time_var a string specifying the time index variable. Defaults to NULL
#' @param values_var a string specifying the values variable. Defaults to NULL
#' @param group_var a string specifying the grouping variable that each unique series sits under. Defaults to NULL
#' @param feature_set the set of time-series features to calculate. Defaults to 'all'
#' @param tsfresh_cleanup a Boolean specifying whether to use the in-built 'tsfresh' relevant feature filter or not. Defaults to FALSE
#' @return object of class DataFrame that contains the summary statistics for each feature
#' @author Trent Henderson
#' @export
#' @examples
#' \dontrun{
#' featMat <- calculate_features(data = simData, 
#'   id_var = "id", 
#'   time_var = "timepoint", 
#'   values_var = "values", 
#'   group_var = "process", 
#'   feature_set = "catch22")
#' }
#'

calculate_features <- function(data, id_var = NULL, time_var = NULL, values_var = NULL, group_var = NULL,
                               feature_set = c("all", "catch22", "feasts", "tsfeatures", "tsfresh", "tsfel"), tsfresh_cleanup = FALSE){
  
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
  
  the_sets <- c("all", "catch22", "feasts", "tsfeatures", "tsfresh", "tsfel")
  '%ni%' <- Negate('%in%')
  
  if(feature_set %ni% the_sets){
    stop("feature_set should be a selection or combination of 'all', 'catch22', 'feasts', 'tsfeatures', 'tsfresh' or 'tsfel' entered as a single string or vector for multiple.")
  }
  
  if(!is.null(group_var) && !is.character(group_var)){
    stop("group_var should be a string specifying the variable name of your grouping variable")
  }
  
  #--------- Feature calcs --------
  
  data_re <- data %>%
    dplyr::rename(id = dplyr::all_of(id_var),
                  timepoint = dplyr::all_of(time_var),
                  values = dplyr::all_of(values_var))
  
  # Group labels
  
  if(!is.null(group_var)){
    
    grouplabs_data <- as.data.frame(data) # Catches cases where input object is of class "tsibble"
    
    grouplabs <- grouplabs_data %>%
      dplyr::rename(id = dplyr::all_of(id_var),
                    group = dplyr::all_of(group_var)) %>%
      dplyr::select(c(id, group)) %>%
      dplyr::distinct() %>%
      dplyr::mutate(id = as.character(id))
  } else{
  }
  
  if("all" %in% feature_set){
    
    message("Calculating all feature sets except for 'tsfresh' and 'tsfel' to avoid Python dependence. If you want these features too, please run the function again specifying 'tsfresh' or 'tsfel' and then append the resultant dataframes.")
    
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
  
  if("tsfresh" %in% feature_set){
    
    message("'tsfresh' requires a Python installation and the 'tsfresh' Python package to also be installed. Please ensure you have this working (see https://tsfresh.com for more information). You can specify which Python to use by running one of the following in your R console/script prior to calling calculate_features(): use_python = 'path_to_your_python_as_a_string_here' or use_virtualenv = 'name_of_your_virtualenv_here'")
    
    if(tsfresh_cleanup == TRUE){
      cleanuper <- "Yes"
    }
    
    if(tsfresh_cleanup == FALSE){
      cleanuper <- "No"
    }
    
    tmp3 <- calc_tsfresh(data = data_re, column_id = "id", column_sort = "timepoint", cleanup = cleanuper)
  }
  
  if("tsfel" %in% feature_set){
    
    message("'tsfel' requires a Python installation and the 'tsfel' Python package to also be installed. Please ensure you have this working (see https://tsfel.readthedocs.io/en/latest/ for more information). You can specify which Python to use by running one of the following in your R console/script prior to calling calculate_features(): use_python = 'path_to_your_python_as_a_string_here' or use_virtualenv = 'name_of_your_virtualenv_here'")
    
    tmp4 <- calc_tsfel(data = data_re)
  }
  
  if(!exists("tmp_all")){
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
    
    if(exists("tmp3")){
      tmp_all <- dplyr::bind_rows(tmp_all, tmp3)
    }
    
    if(exists("tmp4")){
      tmp_all <- dplyr::bind_rows(tmp_all, tmp4)
    }
  } else{
  }
  
  if(exists("grouplabs")){
    tmp_all <- tmp_all %>%
      dplyr::mutate(id = as.character(id)) %>%
      dplyr::left_join(grouplabs, by = c("id" = "id"))
  } else{
  }
  return(tmp_all)
}
