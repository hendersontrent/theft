#------------------- Helper functions to reduce length -------------

#--------
# catch22
#--------

calc_catch22 <- function(data, catch24){
  
  outData <- data %>%
    dplyr::reframe(Rcatch22::catch22_all(!!dplyr::sym(colnames(data)[!colnames(data) %in% append(tsibble::key_vars(data), tsibble::index_var(data))]), 
                                         catch24 = catch24), .by = tsibble::key_vars(data)) %>%
    dplyr::mutate(feature_set = "catch22")
  
  return(outData)
}

#-------
# feasts
#-------

calc_feasts <- function(data){
  
  outData <- data %>%
    fabletools::features(!!dplyr::sym(colnames(data)[!colnames(data) %in% append(tsibble::key_vars(data), tsibble::index_var(data))]),
                         features = fabletools::feature_set(pkgs = "feasts")) %>%
    tidyr::gather("names", "values", -dplyr::all_of(tsibble::key_vars(data))) %>%
    dplyr::mutate(feature_set = "feasts")
  
  return(outData)
}

#-----------
# tsfeatures
#-----------

calc_tsfeatures <- function(data, use_compengine, n_jobs){
  
  if(n_jobs >= 2){
    parallel <- TRUE
  } else{
    parallel <- FALSE
  }
  
  var1 <- tsibble::index_var(data)
  var2 <- colnames(data)[!colnames(data) %in% append(tsibble::key_vars(data), tsibble::index_var(data))]
  var3 <- tsibble::key_vars(data)[1]
  tsf_list <- split(data[, c(var2)], data[, var3])
  
  if(length(tsibble::key_vars(data)) > 1){
    lookup <- data %>%
      as.data.frame() %>%
      dplyr::select(dplyr::all_of(tsibble::key_vars(data))) %>%
      dplyr::distinct()
  }
  
  outData <- lapply(tsf_list, function(x){
    stats::ts(x)
  })
  
  the_names <- data.frame(id = names(outData))
  colnames(the_names) <- var3
  
  featureList <- c("frequency", "stl_features", "entropy", "acf_features",
                   "arch_stat", "crossing_points", "flat_spots",
                   "heterogeneity", "holt_parameters", "hurst", 
                   "lumpiness", "max_kl_shift", "max_level_shift", "max_var_shift", 
                   "nonlinearity", "pacf_features", "stability", "unitroot_kpss",
                   "unitroot_pp", "embed2_incircle", "firstzero_ac",
                   "histogram_mode", "localsimple_taures", "sampenc",
                   "spreadrandomlocal_meantaul")
  
  if(use_compengine){
    featureList <- append(featureList, "compengine")
  }
  
  outData <- try(tsfeatures::tsfeatures(outData, features = featureList, parallel = parallel))
  
  if("try-error" %in% class(outData)){
    message("Removing 'compengine' features from tsfeatures due to error. Recomputing with reduced set...")
    featureList <- featureList[!featureList %in% c("compengine")]
    outData <- tsfeatures::tsfeatures(outData, features = featureList, parallel = parallel)
  }
  
  outData <- cbind(the_names, outData) %>%
    tidyr::pivot_longer(!dplyr::all_of(var3), names_to = "names", values_to = "values") %>%
    dplyr::mutate(feature_set = "tsfeatures")
  
  if(length(tsibble::key_vars(data)) > 1){
    outData <- outData %>%
      dplyr::inner_join(lookup)
  }
  
  return(outData)
}

#--------
# tsfresh
#--------

calc_tsfresh <- function(data, cleanup, n_jobs = 0, warn = TRUE){
  
  mywarn <- ifelse(warn, "Yes", "No")
  
  lookup <- data %>%
    as.data.frame() %>%
    dplyr::select(dplyr::all_of(tsibble::key_vars(data))) %>%
    dplyr::distinct()
  
  # Load Python function
  
  tsfresh_calculator <- function(){}
  reticulate::source_python(system.file("python", "tsfresh_calculator.py", package = "theft")) # Ships with package
    
  temp <- data %>%
    as.data.frame() %>%
    dplyr::group_by(!!dplyr::sym(tsibble::key_vars(data)[1])) %>%
    dplyr::arrange(!!dplyr::sym(tsibble::index_var(data))) %>%
    # dplyr::mutate(timepoint = as.numeric(dplyr::row_number())) %>%
    dplyr::ungroup() %>%
    dplyr::select(!!dplyr::sym(tsibble::key_vars(data)[1]), !!dplyr::sym(tsibble::index_var(data)),
                  !!dplyr::sym(colnames(data)[!colnames(data) %in% append(tsibble::key_vars(data), tsibble::index_var(data))]))
  
  ids <- temp %>%
    dplyr::select(!!dplyr::sym(tsibble::key_vars(data)[1])) %>%
    dplyr::distinct()
    
  outData <- tsfresh_calculator(timeseries = temp, column_id = tsibble::key_vars(data)[1], 
                                column_sort = as.character(dplyr::sym(colnames(data)[!colnames(data) %in% append(tsibble::key_vars(data), tsibble::index_var(data))])), 
                                cleanup = cleanup, n_jobs = n_jobs, warn = mywarn) %>%
    cbind(ids) %>%
    tidyr::gather("names", "values", -tsibble::key_vars(data)[1]) %>%
    dplyr::inner_join(lookup) %>%
    dplyr::mutate(feature_set = "tsfresh")
  
  return(outData)
}

#------
# TSFEL
#------

calc_tsfel <- function(data, n_jobs, warn){
  
  mywarn <- ifelse(warn, "Yes", "No")
  
  # Load Python function
  
  tsfel_calculator <- function(){}
  reticulate::source_python(system.file("python", "tsfel_calculator.py", package = "theft")) # Ships with package
  
  outData <- data %>%
    dplyr::reframe(tsfel_calculator(!!dplyr::sym(colnames(data)[!colnames(data) %in% append(tsibble::key_vars(data), tsibble::index_var(data))]),
                                    n_jobs = n_jobs, warn = mywarn), 
                   .by = dplyr::all_of(tsibble::key_vars(data))) %>%
    tidyr::gather("names", "values", -tsibble::key_vars(data)) %>%
    dplyr::mutate(feature_set = "TSFEL")
  
  return(outData)
}

#-----
# Kats
#-----

calc_kats <- function(data, warn){
  
  mywarn <- ifelse(warn, "Yes", "No")
  
  # Load Python function
  
  kats_calculator <- function(){}
  reticulate::source_python(system.file("python", "kats_calculator.py", package = "theft")) # Ships with package
  
  # Convert numeric time index to datetime as Kats requires it
  
  unique_times <- data %>%
    as.data.frame() %>%
    dplyr::select(!!dplyr::sym(tsibble::index_var(data))) %>%
    dplyr::distinct() %>%
    dplyr::pull(!!dplyr::sym(tsibble::index_var(data)))
  
  datetimes <- data.frame(timepoint = unique_times) %>%
    dplyr::mutate(time = seq(as.Date("1800-01-01"), by = "day", length.out = length(unique_times)))
  
  colnames(datetimes) <- c(tsibble::index_var(data), "time")
  
  # Join in datetimes and run computations
  
  outData <- data %>%
    dplyr::inner_join(datetimes) %>%
    dplyr::select(-!!dplyr::sym(tsibble::index_var(data))) %>%
    dplyr::reframe(results = list(kats_calculator(timepoints = .data$time, 
                                                  values = !!dplyr::sym(colnames(data)[!colnames(data) %in% append(tsibble::key_vars(data), tsibble::index_var(data))]),
                                                  warn = mywarn)), 
                   .by = dplyr::all_of(tsibble::key_vars(data))) %>%
    tidyr::unnest_wider(!!dplyr::sym("results")) %>%
    tidyr::gather("names", "values", -tsibble::key_vars(data)) %>%
    dplyr::mutate(feature_set = "Kats")
  
  return(outData)
}

#-----
# User
#-----

calc_user <- function(data, features){
  
  outData <- data %>%
    dplyr::reframe(dplyr::across(dplyr::all_of(colnames(data)[!colnames(data) %in% append(tsibble::key_vars(data), tsibble::index_var(data))]), 
                                 .fns = features), 
                   .by = tsibble::key_vars(data))
  
  colnames(outData) <- append(tsibble::key_vars(data), names(features))
  key_var_count <- length(tsibble::key_vars(data)) + 1
  col_count <- ncol(outData)
  cols <- key_var_count:col_count
    
  outData <- outData %>%
    tidyr::pivot_longer(cols = dplyr::all_of(cols), names_to = "names", values_to = "values") %>%
    dplyr::mutate(feature_set = "User")
  
  return(outData)
}

#------------------- Main exported calculation function ------------

#' Compute features on an input time series dataset
#' 
#' @importFrom rlang .data :=
#' @importFrom dplyr group_by filter ungroup bind_rows across all_of select rename %>% mutate sym
#' @importFrom tsibble key_vars index_var
#' @param data \code{tbl_ts} containing the time series data
#' @param feature_set \code{character} or \code{vector} of \code{character} denoting the set of time-series features to calculate. Can be one of \code{"catch22"}, \code{"feasts"}, \code{"tsfeatures"}, \code{"tsfresh"}, \code{"tsfel"}, \code{"kats"}, \code{"quantiles"}, and or \code{"moments"}
#' @param features named \code{list} containing a set of user-supplied functions to calculate on \code{data}. Each function should take a single argument which is the time series. Defaults to \code{NULL} for no manually-specified features. Each list entry must have a name as \code{calculate_features} looks for these to name the features. If you don't want to use the existing feature sets and only compute those passed to \code{features}, set \code{feature_set = NULL}
#' @param catch24 \code{Boolean} specifying whether to compute \code{catch24} in addition to \code{catch22} if \code{catch22} is one of the feature sets selected. Defaults to \code{FALSE}
#' @param tsfresh_cleanup \code{Boolean} specifying whether to use the in-built \code{tsfresh} relevant feature filter or not. Defaults to \code{FALSE}
#' @param use_compengine \code{Boolean} specifying whether to use the \code{"compengine"} features in \code{tsfeatures}. Defaults to \code{FALSE} to provide immense computational efficiency benefits
#' @param seed \code{integer} denoting a fixed number for R's random number generator to ensure reproducibility. Defaults to \code{123}
#' @param z_score \code{Boolean} specifying whether to z-score the time-series before computing features. Defaults to \code{FALSE}
#' @param n_jobs \code{integer} denoting the number of parallel processes to use if \code{"tsfresh"} or \code{"tsfel"} are specified in \code{"feature_set"}. Defaults to \code{0} for no parallelisation
#' @param warn \code{Boolean} specifying whether to produce warnings from feature set packages. Defaults to \code{TRUE}
#' @return object of class \code{feature_calculations} that contains the summary statistics for each feature
#' @author Trent Henderson
#' @export
#' @examples
#' featMat <- calculate_features(data = simData, 
#'   feature_set = "catch22")
#'

calculate_features <- function(data, feature_set = c("catch22", "feasts", "tsfeatures", 
                                                     "kats", "tsfresh", "tsfel",
                                                     "quantiles", "moments"), 
                               features = NULL, catch24 = FALSE, 
                               tsfresh_cleanup = FALSE, use_compengine = FALSE, 
                               seed = 123, z_score = FALSE, n_jobs = 0, warn = TRUE){
  
  if(!inherits(data, "tbl_ts")){
    stop("As of v0.8.1 `data` must now be a `tbl_ts object`. Please convert your matrix or dataframe using `tsibble::as_tsibble` and specify your `key` and `index` variables.")
  }
  
  stopifnot(inherits(data, "tbl_ts"))
  
  if(ncol(data) > length(tsibble::key_vars(data)) + length(tsibble::index_var(data)) + 1){
    stop("Multiple measured variables detected. Please ensure there is only one measure variable in `data` outside of the `key` and `index` variables used to create the `tbl_ts` object.")
  }
  
  stopifnot(n_jobs >= 0)
  
  feature_set <- tolower(feature_set) # Standardise names
  
  #--------- Filter out time series with NAs --------
  
  ids_pre <- data %>%
    as.data.frame() %>%
    dplyr::select(dplyr::all_of(tsibble::key_vars(data)[1])) %>%
    dplyr::distinct() %>%
    nrow()
  
  data_re <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(tsibble::key_vars(data)))) %>%
    dplyr::filter(!any(is.na(colnames(data)[!colnames(data) %in% append(tsibble::key_vars(data), tsibble::index_var(data))]))) %>%
    dplyr::ungroup()
  
  lookup2 <- unique(data_re[tsibble::key_vars(data_re)])
  ids_post <- nrow(lookup2)
  
  if(ids_pre != ids_post){
    message(paste0("Removed ", ids_post - ids_pre, " time series due to non-real values."))
  }
  
  if(ids_post == 0){
    stop("No time series remaining to calculate features after removing IDs with non-real values.")
  }
  
  #--------- Normalise data --------
  
  if(z_score){
    data_re <- data_re %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(tsibble::key_vars(data)))) %>%
      dplyr::mutate(!!dplyr::sym(colnames(data)[!colnames(data) %in% append(tsibble::key_vars(data), tsibble::index_var(data))]) := 
                      as.numeric(scale(!!dplyr::sym(colnames(data)[!colnames(data) %in% append(tsibble::key_vars(data), tsibble::index_var(data))])))) %>%
      dplyr::ungroup()
  }
  
  #--------- Feature calcs --------
  
  if("catch22" %in% feature_set){
      message("Running computations for catch22...\n")
    if(!warn){
      tmp_catch22 <-suppressWarnings(calc_catch22(data = data_re, catch24 = catch24))
    } else{
      tmp_catch22 <- calc_catch22(data = data_re, catch24 = catch24)
      }
    }
    
    if("feasts" %in% feature_set){
      message("Running computations for feasts...\n")
      if(!warn){
        tmp_feasts <- suppressWarnings(calc_feasts(data = data_re))
      } else{
        tmp_feasts <- calc_feasts(data = data_re)
      }
    }
    
    if("tsfeatures" %in% feature_set){
      message("Running computations for tsfeatures...\n")
      if(!warn){
        tmp_tsfeatures <- suppressWarnings(calc_tsfeatures(data = data_re, use_compengine = use_compengine, n_jobs = n_jobs))
      } else{
        tmp_tsfeatures <- calc_tsfeatures(data = data_re, use_compengine = use_compengine, n_jobs = n_jobs)
      }
    }
    
    if("tsfresh" %in% feature_set){
      
      if(tsfresh_cleanup){
        cleanuper <- "Yes"
      } else{
        cleanuper <- "No"
      }
      
      message("Running computations for tsfresh...\n")
      if(!warn){
        tmp_tsfresh <- suppressWarnings(calc_tsfresh(data = data_re, cleanup = cleanuper, n_jobs = as.integer(n_jobs), warn = warn))
      } else{
        tmp_tsfresh <- calc_tsfresh(data = data_re, cleanup = cleanuper, n_jobs = as.integer(n_jobs), warn = warn)
      }
    }
    
    if("tsfel" %in% feature_set){
      message("Running computations for TSFEL...\n")
      if(!warn){
        tmp_tsfel <- suppressWarnings(calc_tsfel(data = data_re, n_jobs = as.integer(n_jobs), warn = warn))
      } else{
        tmp_tsfel <- calc_tsfel(data = data_re, n_jobs = as.integer(n_jobs), warn = warn)
      }
    }
    
    if("kats" %in% feature_set){
      message("Running computations for Kats...\n")
      if(!warn){
        tmp_kats <- suppressWarnings(calc_kats(data = data_re, warn = warn))
      } else{
        tmp_kats <- calc_kats(data = data_re, warn = warn)
      }
    }
    
    if("quantiles" %in% feature_set){
      message("Running computations for quantiles...\n")
      if(!warn){
        tmp_quantiles <- suppressWarnings(
          data_re %>%
            dplyr::reframe(quantiles(.data$values), .by = tsibble::key_vars(data))
        )
      } else{
        tmp_quantiles <- data_re %>%
          dplyr::reframe(quantiles(.data$values), .by = tsibble::key_vars(data))
      }
    }
    
    if("moments" %in% feature_set){
      message("Running computations for moments...\n")
      if(!warn){
        tmp_moments <- suppressWarnings(
          data_re %>%
            dplyr::reframe(moments(.data$values), .by = tsibble::key_vars(data))
        )
      } else{
        tmp_moments <- data_re %>%
          dplyr::reframe(moments(.data$values), .by = tsibble::key_vars(data))
      }
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
      
      message("Running computations for user-supplied features...\n")
      tmp_user <- calc_user(data = data_re, features = features)
    }
  
  #--------- Feature binding --------
  
  tmp_all_features <- data.frame()
  
  if(length(feature_set) > 1){
    message("Binding feature dataframes together...\n")
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
  
  if(exists("tmp_quantiles")){
    tmp_all_features <- dplyr::bind_rows(tmp_all_features, tmp_quantiles)
  }
  
  if(exists("tmp_moments")){
    tmp_all_features <- dplyr::bind_rows(tmp_all_features, tmp_moments)
  }
  
  if(exists("tmp_user")){
    tmp_all_features <- dplyr::bind_rows(tmp_all_features, tmp_user)
  }
  
  # Change column names to be consistent with {theftdlc} package
  
  keep_cols <- c("id", "group", "feature_set", "names", "values")
  
  if(length(tsibble::key_vars(data)) > 1){
    tmp_all_features <- tmp_all_features %>%
      dplyr::rename(id = dplyr::all_of(tsibble::key_vars(data)[1]),
                    group = dplyr::all_of(tsibble::key_vars(data)[2])) %>%
      dplyr::select(dplyr::all_of(keep_cols))
  } else{
    if(tsibble::key_vars(data)[1] != "id"){
      tmp_all_features <- tmp_all_features %>%
        dplyr::rename(id = dplyr::all_of(tsibble::key_vars(data)[1]))
    }
  }
  
  tmp_all_features <- structure(tmp_all_features, class = c("feature_calculations", "data.frame"))
  return(tmp_all_features)
}
