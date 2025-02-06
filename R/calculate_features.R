#------------------- Helper functions to reduce length -------------

#--------
# catch22
#--------

calc_catch22 <- function(data, catch24){
  
  if("group" %in% colnames(data)){
    outData <- data %>%
      tibble::as_tibble() %>%
      dplyr::group_by(.data$id) %>%
      dplyr::arrange(.data$timepoint) %>%
      dplyr::ungroup() %>%
      dplyr::reframe(Rcatch22::catch22_all(.data$values, catch24 = catch24), .by = c(.data$id, .data$group)) %>%
      dplyr::mutate(feature_set = "catch22")
  } else{
    outData <- data %>%
      tibble::as_tibble() %>%
      dplyr::group_by(.data$id) %>%
      dplyr::arrange(.data$timepoint) %>%
      dplyr::ungroup() %>%
      dplyr::reframe(Rcatch22::catch22_all(.data$values, catch24 = catch24), .by = c(.data$id)) %>%
      dplyr::mutate(feature_set = "catch22")
  }
  
  return(outData)
}

#-------
# feasts
#-------

calc_feasts <- function(data){
  
  if("group" %in% colnames(data)){
    tsData <- tsibble::as_tsibble(data, key = c(.data$id, .data$group), index = .data$timepoint)
    
    outData <- tsData %>%
      fabletools::features(.data$values, fabletools::feature_set(pkgs = "feasts"))  %>%
      tidyr::gather("names", "values", -c(.data$id, .data$group)) %>%
      dplyr::mutate(feature_set = "feasts")
  } else{
    tsData <- tsibble::as_tsibble(data, key = c(.data$id), index = .data$timepoint)
    
    outData <- tsData %>%
      fabletools::features(.data$values, fabletools::feature_set(pkgs = "feasts"))  %>%
      tidyr::gather("names", "values", -.data$id) %>%
      dplyr::mutate(feature_set = "feasts")
  }
  
  return(outData)
}

#-----------
# tsfeatures
#-----------

tsfeatures_helper <- function(data, grouped = FALSE, feats, parallel){
  
  if(grouped){
    vars <- c("id", "group")
  } else{
    vars <- c("id")
  }
  
  outData <- data %>%
    tibble::as_tibble() %>%
    dplyr::group_by_at(dplyr::all_of(vars)) %>%
    dplyr::arrange(.data$timepoint) %>%
    dplyr::select(-c(.data$timepoint)) %>%
    dplyr::ungroup() %>%
    dplyr::reframe(values = list(.data$values), .by = dplyr::all_of(vars)) %>%
    dplyr::reframe(tsfeatures::tsfeatures(.data$values, features = feats), .by = dplyr::all_of(vars)) %>%
    tidyr::gather("names", "values", -c(dplyr::all_of(vars))) %>%
    dplyr::mutate(feature_set = "tsfeatures")
  
  return(outData)
}

calc_tsfeatures <- function(data){
  
  featureList <- c("frequency", "stl_features", "entropy", "acf_features",
                   "compengine", "arch_stat", "crossing_points", "flat_spots",
                   "heterogeneity", "holt_parameters", "hurst", 
                   "lumpiness", "max_kl_shift", "max_level_shift", "max_var_shift", 
                   "nonlinearity", "pacf_features", "stability", "unitroot_kpss",
                   "unitroot_pp", "embed2_incircle", "firstzero_ac",
                   "histogram_mode", "localsimple_taures", "sampenc",
                   "spreadrandomlocal_meantaul")
  
  if("group" %in% colnames(data)){
    outData <- try(tsfeatures_helper(data = data, grouped = TRUE, feats = featureList))
    
    if("try-error" %in% class(outData)){
      
      message("Removing 'compengine' features from tsfeatures due to length error. Recomputing with reduced set...")
      featureList <- featureList[!featureList %in% c("compengine")]
      outData <- try(tsfeatures_helper(data = data, grouped = TRUE, feats = featureList))
    }
    
  } else{
    outData <- try(tsfeatures_helper(data = data, grouped = FALSE, feats = featureList))
    
    if("try-error" %in% class(outData)){
      
      message("Removing 'compengine' features from tsfeatures due to length error. Recomputing with reduced set...")
      featureList <- featureList[!featureList %in% c("compengine")]
      outData <- try(tsfeatures_helper(data = data, grouped = FALSE, feats = featureList))
    }
  }
  
  return(outData)
}

#--------
# tsfresh
#--------

calc_tsfresh <- function(data, column_id = "id", column_sort = "timepoint", cleanup){
  
  if("group" %in% colnames(data)){
    groups <- data %>%
      dplyr::select(c(.data$id, .data$group)) %>%
      dplyr::distinct()
  }
  
  # Load Python function
  
  tsfresh_calculator <- function(){}
  reticulate::source_python(system.file("python", "tsfresh_calculator.py", package = "theft")) # Ships with package
  
  # Convert time index column to numeric to avoid {tsfresh} errors
  
  if(!is.numeric(data$id) || !is.numeric(data$timepoint)){
    
    ids <- data.frame(old_id = unique(data$id)) %>%
      dplyr::mutate(id = dplyr::row_number())
    
    temp <- data %>%
      dplyr::rename(old_id = id) %>%
      dplyr::left_join(ids, by = c("old_id" = "old_id")) %>%
      dplyr::group_by(.data$id) %>%
      dplyr::arrange(.data$timepoint) %>%
      dplyr::mutate(timepoint = as.numeric(dplyr::row_number())) %>%
      dplyr::ungroup()
    
    # Dropping columns with dplyr::select() isn't working, so just make a new dataframe
    
    temp1 <- data.frame(id = temp$id,
                        timepoint = temp$timepoint,
                        values = temp$values)
    
    if("group" %in% colnames(data)){
      
      classes <- groups %>%
        dplyr::select(c(.data$group)) %>%
        dplyr::mutate(id = dplyr::row_number())
      
      outData <- tsfresh_calculator(timeseries = temp1, column_id = column_id, column_sort = column_sort, cleanup = cleanup, classes = classes)
    } else{
      outData <- tsfresh_calculator(timeseries = temp1, column_id = column_id, column_sort = column_sort, cleanup = cleanup)
    }
    
    # Compute features and re-join back correct id labels
    
    ids2 <- ids %>%
      dplyr::select(-c(.data$id)) %>%
      dplyr::rename(id = .data$old_id)
    
    outData <- outData %>%
      cbind(ids2) %>%
      tidyr::gather("names", "values", -.data$id) %>%
      dplyr::mutate(feature_set = "tsfresh")
    
  } else{
    temp1 <- data.frame(id = data$id,
                        timepoint = data$timepoint,
                        values = data$values)
    
    ids <- unique(temp1$id)
    
    if("group" %in% colnames(data)){
      
      classes <- groups %>%
        dplyr::select(c(.data$group)) %>%
        dplyr::mutate(id = dplyr::row_number())
      
      outData <- tsfresh_calculator(timeseries = temp1, column_id = column_id, column_sort = column_sort, cleanup = cleanup, clases = classes) 
    } else{
      outData <- tsfresh_calculator(timeseries = temp1, column_id = column_id, column_sort = column_sort, cleanup = cleanup) 
    }
    
    # Do calculations
    
    outData <- outData %>%
      dplyr::mutate(id = ids) %>%
      tidyr::gather("names", "values", -.data$id) %>%
      dplyr::mutate(feature_set = "tsfresh")
  }
  
  if(c("group") %in% colnames(data)){
    outData <- outData %>%
      dplyr::inner_join(groups, by = c("id" = "id"))
  }
  
  return(outData)
}

#------
# TSFEL
#------

calc_tsfel <- function(data){
  
  # Load Python function
  
  tsfel_calculator <- function(){}
  reticulate::source_python(system.file("python", "tsfel_calculator.py", package = "theft")) # Ships with package
  
  if("group" %in% colnames(data)){
    outData <- data %>%
      tibble::as_tibble() %>%
      dplyr::group_by(.data$id, .data$group) %>%
      dplyr::arrange(.data$timepoint) %>%
      dplyr::ungroup() %>%
      dplyr::reframe(tsfel_calculator(.data$values), .by = c(.data$id, .data$group)) %>%
      tidyr::gather("names", "values", -c(.data$id, .data$group)) %>%
      dplyr::mutate(feature_set = "TSFEL")
  } else{
    outData <- data %>%
      tibble::as_tibble() %>%
      dplyr::group_by(.data$id) %>%
      dplyr::arrange(.data$timepoint) %>%
      dplyr::ungroup() %>%
      dplyr::reframe(tsfel_calculator(.data$values), .by = c(.data$id)) %>%
      tidyr::gather("names", "values", -c(.data$id)) %>%
      dplyr::mutate(feature_set = "TSFEL")
  }
  
  return(outData)
}

#-----
# Kats
#-----

calc_kats <- function(data){
  
  # Load Python function
  
  kats_calculator <- function(){}
  reticulate::source_python(system.file("python", "kats_calculator.py", package = "theft")) # Ships with package
  
  # Convert numeric time index to datetime as Kats requires it
  
  unique_times <- unique(data$timepoint)
  
  datetimes <- data.frame(timepoint = unique_times) %>%
    dplyr::mutate(time = seq(as.Date("1800-01-01"), by = "day", length.out = length(unique_times)))
  
  # Join in datetimes and run computations
  
  if("group" %in% colnames(data)){
    outData <- data %>%
      dplyr::left_join(datetimes, by = c("timepoint" = "timepoint")) %>%
      dplyr::select(-c(.data$timepoint)) %>%
      dplyr::group_by(.data$id, .data$group) %>%
      dplyr::arrange(.data$time) %>%
      dplyr::ungroup() %>%
      dplyr::reframe(results = list(kats_calculator(timepoints = .data$time, values = .data$values)), .by = c(.data$id, .data$group)) %>%
      tidyr::unnest_wider(.data$results) %>%
      tidyr::gather("names", "values", -c(.data$id, .data$group)) %>%
      dplyr::mutate(feature_set = "Kats")
  } else{
    outData <- data %>%
      dplyr::left_join(datetimes, by = c("timepoint" = "timepoint")) %>%
      dplyr::select(-c(.data$timepoint)) %>%
      dplyr::group_by(.data$id) %>%
      dplyr::arrange(.data$time) %>%
      dplyr::ungroup() %>%
      dplyr::reframe(results = list(kats_calculator(timepoints = .data$time, values = .data$values)), .by = c(.data$id)) %>%
      tidyr::unnest_wider(.data$results) %>%
      tidyr::gather("names", "values", -c(.data$id)) %>%
      dplyr::mutate(feature_set = "Kats")
  }
  
  return(outData)
}

#-----
# User
#-----

calc_user <- function(data, features){
  
  if("group" %in% colnames(data)){
    outData <- data %>%
      tibble::as_tibble() %>%
      dplyr::group_by(.data$id) %>%
      dplyr::arrange(.data$timepoint) %>%
      dplyr::ungroup() %>%
      dplyr::reframe(dplyr::across(.data$values, .fns = features), .by = c(.data$id, .data$group))
    
    colnames(outData) <- append(c("id", "group"), names(features))
    
    outData <- outData %>%
      tidyr::pivot_longer(cols = 3:ncol(outData), names_to = "names", values_to = "values") %>%
      dplyr::mutate(feature_set = "User")
  } else{
    outData <- data %>%
      tibble::as_tibble() %>%
      dplyr::group_by(.data$id) %>%
      dplyr::arrange(.data$timepoint) %>%
      dplyr::ungroup() %>%
      dplyr::reframe(dplyr::across(.data$values, .fns = features), .by = c(.data$id))
    
    colnames(outData) <- append(c("id"), names(features))
    
    outData <- outData %>%
      tidyr::pivot_longer(cols = 2:ncol(outData), names_to = "names", values_to = "values") %>%
      dplyr::mutate(feature_set = "User")
  }
  
  return(outData)
}

#------------------- Main exported calculation function ------------

#' Compute features on an input time series dataset
#' 
#' @importFrom rlang .data
#' @importFrom dplyr group_by filter ungroup bind_rows
#' @import tsibble key_vars index_var
#' @param data \code{tbl_ts} containing the time series data
#' @param feature_set \code{character} or \code{vector} of \code{character} denoting the set of time-series features to calculate. Defaults to \code{"catch22"}
#' @param catch24 \code{Boolean} specifying whether to compute \code{catch24} in addition to \code{catch22} if \code{catch22} is one of the feature sets selected. Defaults to \code{FALSE}
#' @param tsfresh_cleanup \code{Boolean} specifying whether to use the in-built \code{tsfresh} relevant feature filter or not. Defaults to \code{FALSE}
#' @param features named \code{list} containing a set of user-supplied functions to calculate on \code{data}. Each function should take a single argument which is the time series. Defaults to \code{NULL} for no manually-specified features. Each list entry must have a name as \code{calculate_features} looks for these to name the features. If you don't want to use the existing feature sets and only compute those passed to \code{features}, set \code{feature_set = NULL}
#' @param seed \code{integer} denoting a fixed number for R's random number generator to ensure reproducibility. Defaults to \code{123}
#' @param parallel \code{Boolean} denoting whether to use parallel processing. Defaults to \code{FALSE}. Recommended if features are to be calculated for numerous time series
#' @return object of class \code{feature_calculations} that contains the summary statistics for each feature
#' @author Trent Henderson
#' @export
#' @examples
#' featMat <- calculate_features(data = simData, 
#'   feature_set = "catch22",
#'   seed = 123)
#'

calculate_features <- function(data, feature_set = c("catch22", "feasts", "tsfeatures", 
                                                     "kats", "tsfresh", "tsfel"), 
                               catch24 = FALSE, tsfresh_cleanup = FALSE, features = NULL, seed = 123,
                               parallel = FALSE, n_procs = 1){
  
  if(!inherits(data, "tbl_ts")){
    stop("As of v0.7.2 'data' must now be a `tbl_ts object`. Please convert your matrix or dataframe using `tsibble::as_tsibble` and specify your `key` and `index` variables.")
  }
  
  stopifnot(inherits(data, "tbl_ts"))
  
  if(ncol(data) > length(tsibble::key_vars(data) + tsibble::index_var(data) + 1)){
    stop("Multiple measured variables detected. Please ensure there is only one measure variable in `data` outside of the `key` and `index` variables used to create the `tbl_ts` object.")
  }
  
  feature_set <- tolower(feature_set) # Standardise names
  
  #--------- Create lookup table --------
  
  lookup <- unique(data[tsibble::key_vars(data)])
  
  #--------- Filter out time series with NAs --------
  
  ids_pre <- nrow(lookup)
  
  data_re <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(tsibble::key_vars(data)))) |>
    dplyr::filter(!any(is.na(colnames(data)[!colnames(data) %in% append(tsibble::key_vars(data), tsibble::index_var(data))]))) |>
    dplyr::ungroup()
  
  lookup2 <- unique(data_re[tsibble::key_vars(data_re)])
  ids_post <- nrow(lookup2)
  
  if(ids_pre != ids_post){
    message(paste0("Removed ", ids_post - ids_pre, " time series due to non-real values."))
  }
  
  if(ids_post == 0){
    stop("No time series remaining to calculate features after removing IDs with non-real values.")
  }
  
  #--------- Feature calcs --------
  
  if("catch22" %in% feature_set){
    cat("Running computations for catch22...\n")
    tmp_catch22 <- calc_catch22(data = data_re, catch24 = catch24)
  }
  
  if("feasts" %in% feature_set){
    cat("Running computations for feasts...\n")
    tmp_feasts <- calc_feasts(data = data_re)
  }
  
  if("tsfeatures" %in% feature_set){
    cat("Running computations for tsfeatures...\n")
    tmp_tsfeatures <- calc_tsfeatures(data = data_re, parallel = tsfeatures_parallel)
  }
  
  if("tsfresh" %in% feature_set){
    
    if(tsfresh_cleanup){
      cleanuper <- "Yes"
    } else{
      cleanuper <- "No"
    }
    
    cat("Running computations for tsfresh...\n")
    tmp_tsfresh <- calc_tsfresh(data = data_re, column_id = "id", column_sort = "timepoint", cleanup = cleanuper)
  }
  
  if("tsfel" %in% feature_set){
    cat("Running computations for TSFEL...\n")
    tmp_tsfel <- calc_tsfel(data = data_re)
  }
  
  if("kats" %in% feature_set){
    cat("Running computations for Kats...\n")
    tmp_kats <- calc_kats(data = data_re)
  }
  
  #-----------------------
  # User-supplied features
  #-----------------------
  
  if(!is.null(features)){
    stopifnot(class(features) == "list")
    stopifnot(sapply(features, class) == "function")
    
    if(is.null(names(features))){
      stop("features must be a named list as calculate_features uses the names to label features produced by each function in the list") # More informative error message than above as this is a bit more specific
    }
    
    cat("Running computations for user-supplied features...\n")
    tmp_user <- calc_user(data = data_re, features = features)
  }
  
  #--------- Feature binding --------
  
  tmp_all_features <- data.frame()
  
  if(length(feature_set) > 1){
    cat("Binding feature dataframes together...\n")
  }
  
  if(exists("tmp_catch22")){
    tmp_all_features <- dplyr::bind_rows(tmp_all_features, tmp_catch22)
  }
  
  if(exists("tmp_feasts")){
    tmp_all_features <- dplyr::bind_rows(tmp_all_features, tmp_feasts)
  }
  
  if(exists("tmp_tsfeatures")){
    tmp_all_features <- dplyr::bind_rows(tmp_all_features, tmp_tsfeatures)
  }
  
  if(exists("tmp_tsfresh")){
    tmp_all_features <- dplyr::bind_rows(tmp_all_features, tmp_tsfresh)
  }
  
  if(exists("tmp_tsfel")){
    tmp_all_features <- dplyr::bind_rows(tmp_all_features, tmp_tsfel)
  }
  
  if(exists("tmp_kats")){
    tmp_all_features <- dplyr::bind_rows(tmp_all_features, tmp_kats)
  }
  
  if(exists("tmp_user")){
    tmp_all_features <- dplyr::bind_rows(tmp_all_features, tmp_user)
  }
  
  tmp_all_features <- structure(tmp_all_features, class = c("feature_calculations", "data.frame"))
  return(tmp_all_features)
}
