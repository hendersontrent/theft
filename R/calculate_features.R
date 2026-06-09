#------------------- Helper functions to reduce length -------------

#--------
# catch22
#--------

calc_catch22 <- function(data, catch24){
  key_vars <- tsibble::key_vars(data)
  vals_col <- colnames(data)[!colnames(data) %in% c(key_vars, tsibble::index_var(data))]

  outData <- data %>%
    dplyr::reframe(Rcatch22::catch22_all(!!dplyr::sym(vals_col),
                                         catch24 = catch24), .by = key_vars) %>%
    dplyr::mutate(feature_set = "catch22")

  return(outData)
}

#-------
# feasts
#-------

calc_feasts <- function(data){
  key_vars <- tsibble::key_vars(data)
  vals_col <- colnames(data)[!colnames(data) %in% c(key_vars, tsibble::index_var(data))]

  outData <- data %>%
    fabletools::features(!!dplyr::sym(vals_col),
                         features = fabletools::feature_set(pkgs = "feasts")) %>%
    tidyr::gather("names", "values", -dplyr::all_of(key_vars)) %>%
    dplyr::mutate(feature_set = "feasts")

  return(outData)
}

#-----------
# tsfeatures
#-----------

calc_tsfeatures <- function(data, use_compengine, n_jobs){
  parallel <- n_jobs >= 2

  key_vars <- tsibble::key_vars(data)
  vals_col <- colnames(data)[!colnames(data) %in% c(key_vars, tsibble::index_var(data))]
  var3 <- key_vars[1]

  tsf_list <- split(data[, c(vals_col)], data[, var3])

  if(length(key_vars) > 1){
    lookup <- data %>%
      as.data.frame() %>%
      dplyr::select(dplyr::all_of(key_vars)) %>%
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

  if(length(key_vars) > 1){
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

  key_vars <- tsibble::key_vars(data)
  idx_var <- tsibble::index_var(data)
  key_var1 <- key_vars[1]
  vals_col <- colnames(data)[!colnames(data) %in% c(key_vars, idx_var)]

  lookup <- data %>%
    as.data.frame() %>%
    dplyr::select(dplyr::all_of(key_vars)) %>%
    dplyr::distinct()

  tsfresh_calculator <- function(){}
  reticulate::source_python(system.file("python", "tsfresh_calculator.py", package = "theft"))

  temp <- data %>%
    as.data.frame() %>%
    dplyr::group_by(!!dplyr::sym(key_var1)) %>%
    dplyr::arrange(!!dplyr::sym(idx_var)) %>%
    dplyr::ungroup() %>%
    dplyr::select(!!dplyr::sym(key_var1), !!dplyr::sym(idx_var), !!dplyr::sym(vals_col))

  ids <- temp %>%
    dplyr::select(!!dplyr::sym(key_var1)) %>%
    dplyr::distinct()

  outData <- tsfresh_calculator(timeseries = temp, column_id = key_var1,
                                column_sort = idx_var,
                                cleanup = cleanup, n_jobs = n_jobs, warn = mywarn) %>%
    cbind(ids) %>%
    tidyr::gather("names", "values", -dplyr::all_of(key_var1)) %>%
    dplyr::inner_join(lookup) %>%
    dplyr::mutate(feature_set = "tsfresh")

  return(outData)
}

#------
# TSFEL
#------

calc_tsfel <- function(data, n_jobs, warn){
  mywarn <- ifelse(warn, "Yes", "No")

  key_vars <- tsibble::key_vars(data)
  vals_col <- colnames(data)[!colnames(data) %in% c(key_vars, tsibble::index_var(data))]

  tsfel_calculator <- function(){}
  reticulate::source_python(system.file("python", "tsfel_calculator.py", package = "theft"))

  outData <- data %>%
    dplyr::reframe(tsfel_calculator(!!dplyr::sym(vals_col),
                                    n_jobs = n_jobs, warn = mywarn),
                   .by = dplyr::all_of(key_vars)) %>%
    tidyr::gather("names", "values", -dplyr::all_of(key_vars)) %>%
    dplyr::mutate(feature_set = "TSFEL")

  return(outData)
}

#-----
# Kats
#-----

calc_kats <- function(data, warn){
  mywarn <- ifelse(warn, "Yes", "No")

  key_vars <- tsibble::key_vars(data)
  idx_var <- tsibble::index_var(data)
  vals_col <- colnames(data)[!colnames(data) %in% c(key_vars, idx_var)]

  kats_calculator <- function(){}
  reticulate::source_python(system.file("python", "kats_calculator.py", package = "theft"))

  unique_times <- data %>%
    as.data.frame() %>%
    dplyr::select(!!dplyr::sym(idx_var)) %>%
    dplyr::distinct() %>%
    dplyr::pull(!!dplyr::sym(idx_var))

  datetimes <- data.frame(timepoint = unique_times) %>%
    dplyr::mutate(time = seq(as.Date("1800-01-01"), by = "day", length.out = length(unique_times)))

  colnames(datetimes) <- c(idx_var, "time")

  outData <- data %>%
    dplyr::inner_join(datetimes) %>%
    dplyr::select(-!!dplyr::sym(idx_var)) %>%
    dplyr::reframe(results = list(kats_calculator(timepoints = .data$time,
                                                  values = !!dplyr::sym(vals_col),
                                                  warn = mywarn)),
                   .by = dplyr::all_of(key_vars)) %>%
    tidyr::unnest_wider(!!dplyr::sym("results")) %>%
    tidyr::gather("names", "values", -dplyr::all_of(key_vars)) %>%
    dplyr::mutate(feature_set = "Kats")

  return(outData)
}

#------
# hctsa
#------

calc_hctsa <- function(data, warn, n_jobs = 0){
  mywarn <- ifelse(warn, "Yes", "No")

  key_vars <- tsibble::key_vars(data)

  pyhctsa_calculator <- function(){}
  reticulate::source_python(system.file("python", "pyhctsa_calculator.py", package = "theft"))
  hctsa_config <- system.file("yaml", "hctsa.yaml", package = "theft")

  sel_cols <- c("id", "timepoint", "values")

  ts_list <- data %>%
    as.data.frame() %>%
    dplyr::select(dplyr::all_of(sel_cols)) %>%
    tidyr::pivot_wider(id_cols = "id", names_from = "timepoint", values_from = "values")

  idx <- ts_list$id

  ts_list <- ts_list %>%
    dplyr::select(-c("id")) %>%
    as.matrix()

  outData <- pyhctsa_calculator(ts_list, mywarn, hctsa_config, n_jobs = as.integer(n_jobs))

  outData <- outData %>%
    dplyr::mutate(id = idx)

  if(length(key_vars) > 1){

    group_labs <- data %>%
      as.data.frame() %>%
      dplyr::select(dplyr::all_of(key_vars)) %>%
      dplyr::distinct()

    outData <- outData %>%
      dplyr::inner_join(group_labs, by = c("id" = "id"))
  }

  outData <- outData %>%
    dplyr::select(c(dplyr::where(is.numeric), dplyr::all_of(key_vars))) %>%
    tidyr::pivot_longer(cols = !dplyr::all_of(key_vars), names_to = "names", values_to = "values") %>%
    dplyr::mutate(feature_set = "hctsa")

  return(outData)
}

#-----
# User
#-----

calc_user <- function(data, features){
  key_vars <- tsibble::key_vars(data)
  vals_col <- colnames(data)[!colnames(data) %in% c(key_vars, tsibble::index_var(data))]

  outData <- data %>%
    dplyr::reframe(dplyr::across(dplyr::all_of(vals_col), .fns = features),
                   .by = key_vars)

  colnames(outData) <- c(key_vars, names(features))
  key_var_count <- length(key_vars) + 1
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
#' @param feature_set \code{character} or \code{vector} of \code{character} denoting the set of time-series features to calculate. Can be one of \code{"catch22"}, \code{"feasts"}, \code{"tsfeatures"}, \code{"tsfresh"}, \code{"tsfel"}, \code{"kats"}, \code{"quantiles"}, \code{"moments"}, \code{"fftquantiles"}, and or \code{"fft"}
#' @param features named \code{list} containing a set of user-supplied functions to calculate on \code{data}. Each function should take a single argument which is the time series. Defaults to \code{NULL} for no manually-specified features. Each list entry must have a name as \code{calculate_features} looks for these to name the features. If you don't want to use the existing feature sets and only compute those passed to \code{features}, set \code{feature_set = NULL}
#' @param catch24 \code{Boolean} specifying whether to compute \code{catch24} in addition to \code{catch22} if \code{catch22} is one of the feature sets selected. Defaults to \code{FALSE}
#' @param tsfresh_cleanup \code{Boolean} specifying whether to use the in-built \code{tsfresh} relevant feature filter or not. Defaults to \code{FALSE}
#' @param use_compengine \code{Boolean} specifying whether to use the \code{"compengine"} features in \code{tsfeatures}. Defaults to \code{FALSE} to provide immense computational efficiency benefits
#' @param seed \code{integer} denoting a fixed number for R's random number generator to ensure reproducibility. Defaults to \code{123}
#' @param z_score \code{Boolean} specifying whether to z-score the time-series before computing features. Defaults to \code{FALSE}
#' @param n_jobs \code{integer} denoting the number of parallel processes to use. Defaults to \code{0} for no parallelisation
#' @param squared \code{Boolean} specifying whether to compute squared magnitude (\code{|X[k]|^2}) for the \code{"fft"} and \code{"fftquantiles"} feature sets if specified. Defaults to \code{TRUE}
#' @param warn \code{Boolean} specifying whether to produce warnings from feature set packages. Defaults to \code{TRUE}
#' @return object of class \code{feature_calculations} that contains the summary statistics for each feature
#' @author Trent Henderson
#' @export
#' @examples
#' features <- calculate_features(data = simData,
#'   feature_set = "catch22")
#'

calculate_features <- function(data, feature_set = c("catch22", "feasts", "tsfeatures",
                                                     "kats", "tsfresh", "tsfel", "hctsa",
                                                     "quantiles", "moments", "fftquantiles",
                                                     "fft"),
                               features = NULL, catch24 = FALSE,
                               tsfresh_cleanup = FALSE, use_compengine = FALSE,
                               seed = 123, z_score = FALSE, squared = TRUE, n_jobs = 0,
                               warn = TRUE){

  if(!inherits(data, "tbl_ts")){
    stop("As of v0.8.1 `data` must now be a `tbl_ts object`. Please convert your matrix or dataframe using `tsibble::as_tsibble` and specify your `key` and `index` variables.")
  }

  stopifnot(inherits(data, "tbl_ts"))

  key_vars <- tsibble::key_vars(data)
  idx_var <- tsibble::index_var(data)
  vals_col <- colnames(data)[!colnames(data) %in% c(key_vars, idx_var)]

  if(ncol(data) > length(key_vars) + length(idx_var) + 1){
    stop("Multiple measured variables detected. Please ensure there is only one measure variable in `data` outside of the `key` and `index` variables used to create the `tbl_ts` object.")
  }

  stopifnot(n_jobs >= 0)

  feature_set <- tolower(feature_set) # Standardise names

  #--------- Filter out time series with NAs --------

  ids_pre <- data %>%
    as.data.frame() %>%
    dplyr::select(dplyr::all_of(key_vars[1])) %>%
    dplyr::distinct() %>%
    nrow()

  data_re <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(key_vars))) %>%
    dplyr::filter(!any(is.na(vals_col))) %>%
    dplyr::ungroup()

  lookup2 <- unique(data_re[key_vars])
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
      dplyr::group_by(dplyr::across(dplyr::all_of(key_vars))) %>%
      dplyr::mutate(!!dplyr::sym(vals_col) := as.numeric(scale(!!dplyr::sym(vals_col)))) %>%
      dplyr::ungroup()
  }

  #--------- Feature calcs --------

  results <- list()

  if("catch22" %in% feature_set){
    message("Running computations for catch22...\n")
    if(!warn){
      results[["catch22"]] <- suppressWarnings(calc_catch22(data = data_re, catch24 = catch24))
    } else{
      results[["catch22"]] <- calc_catch22(data = data_re, catch24 = catch24)
    }
  }

  if("feasts" %in% feature_set){
    message("Running computations for feasts...\n")
    if(!warn){
      results[["feasts"]] <- suppressWarnings(calc_feasts(data = data_re))
    } else{
      results[["feasts"]] <- calc_feasts(data = data_re)
    }
  }

  if("tsfeatures" %in% feature_set){
    message("Running computations for tsfeatures...\n")
    if(!warn){
      results[["tsfeatures"]] <- suppressWarnings(calc_tsfeatures(data = data_re, use_compengine = use_compengine, n_jobs = n_jobs))
    } else{
      results[["tsfeatures"]] <- calc_tsfeatures(data = data_re, use_compengine = use_compengine, n_jobs = n_jobs)
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
      results[["tsfresh"]] <- suppressWarnings(calc_tsfresh(data = data_re, cleanup = cleanuper, n_jobs = as.integer(n_jobs), warn = warn))
    } else{
      results[["tsfresh"]] <- calc_tsfresh(data = data_re, cleanup = cleanuper, n_jobs = as.integer(n_jobs), warn = warn)
    }
  }

  if("tsfel" %in% feature_set){
    message("Running computations for TSFEL...\n")
    if(!warn){
      results[["tsfel"]] <- suppressWarnings(calc_tsfel(data = data_re, n_jobs = as.integer(n_jobs), warn = warn))
    } else{
      results[["tsfel"]] <- calc_tsfel(data = data_re, n_jobs = as.integer(n_jobs), warn = warn)
    }
  }

  if("kats" %in% feature_set){
    message("Running computations for Kats...\n")
    if(!warn){
      results[["kats"]] <- suppressWarnings(calc_kats(data = data_re, warn = warn))
    } else{
      results[["kats"]] <- calc_kats(data = data_re, warn = warn)
    }
  }

  if("hctsa" %in% feature_set){
    message("Running computations for hctsa...\n")
    if(!warn){
      results[["hctsa"]] <- suppressWarnings(calc_hctsa(data = data_re, warn = warn, n_jobs = as.integer(n_jobs)))
    } else{
      results[["hctsa"]] <- calc_hctsa(data = data_re, warn = warn, n_jobs = as.integer(n_jobs))
    }
  }

  if("quantiles" %in% feature_set){
    message("Running computations for quantiles...\n")
    if(!warn){
      results[["quantiles"]] <- suppressWarnings(
        data_re %>%
          dplyr::reframe(quantiles(!!dplyr::sym(vals_col)), .by = key_vars)
      )
    } else{
      results[["quantiles"]] <- data_re %>%
        dplyr::reframe(quantiles(!!dplyr::sym(vals_col)), .by = key_vars)
    }
  }

  if("moments" %in% feature_set){
    message("Running computations for moments...\n")
    if(!warn){
      results[["moments"]] <- suppressWarnings(
        data_re %>%
          dplyr::reframe(moments(!!dplyr::sym(vals_col)), .by = key_vars)
      )
    } else{
      results[["moments"]] <- data_re %>%
        dplyr::reframe(moments(!!dplyr::sym(vals_col)), .by = key_vars)
    }
  }

  if("fftquantiles" %in% feature_set){
    message("Running computations for fftquantiles...\n")
    if(!warn){
      results[["fftquantiles"]] <- suppressWarnings(
        data_re %>%
          dplyr::reframe(fftquantiles(!!dplyr::sym(vals_col), squared = squared), .by = key_vars)
      )
    } else{
      results[["fftquantiles"]] <- data_re %>%
        dplyr::reframe(fftquantiles(!!dplyr::sym(vals_col), squared = squared), .by = key_vars)
    }
  }

  if("fft" %in% feature_set){
    message("Running computations for fft...\n")
    if(!warn){
      results[["fft"]] <- suppressWarnings(
        data_re %>%
          dplyr::reframe(fft_features(!!dplyr::sym(vals_col), squared = squared), .by = key_vars)
      )
    } else{
      results[["fft"]] <- data_re %>%
        dplyr::reframe(fft_features(!!dplyr::sym(vals_col), squared = squared), .by = key_vars)
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
    results[["user"]] <- calc_user(data = data_re, features = features)
  }

  #--------- Feature binding --------

  if(length(results) > 1){
    message("Binding feature dataframes together...\n")
  }

  tmp_all_features <- dplyr::bind_rows(results)

  # Change column names to be consistent with {theftdlc} package

  keep_cols <- c("id", "group", "feature_set", "names", "values")

  if(length(key_vars) > 1){
    tmp_all_features <- tmp_all_features %>%
      dplyr::rename(id = dplyr::all_of(key_vars[1]),
                    group = dplyr::all_of(key_vars[2])) %>%
      dplyr::select(dplyr::all_of(keep_cols))
  } else{
    if(key_vars[1] != "id"){
      tmp_all_features <- tmp_all_features %>%
        dplyr::rename(id = dplyr::all_of(key_vars[1]))
    }
  }

  tmp_all_features <- structure(tmp_all_features, class = c("feature_calculations", "data.frame"))
  return(tmp_all_features)
}
