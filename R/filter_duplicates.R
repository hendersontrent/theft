#' Remove duplicate features that exist in multiple feature sets and retain a reproducible random selection of one of them
#' 
#' @importFrom stats na.omit
#' @import dplyr
#' 
#' @param data \code{feature_calculations} object containing the raw feature matrix produced by \code{calculate_features}
#' @param preference deprecated. Do not use
#' @param seed \code{integer} denoting a fix for R's pseudo-random number generator to ensure selections are reproducible. Defaults to \code{123}
#' @return \code{feature_calculations} object containing filtered feature data
#' @author Trent Henderson
#' @export
#' 

filter_duplicates <- function(data, preference = NULL, seed = 123){
  
  '%ni%' <- Negate('%in%')
  
  if(sum(c("catch22", "feasts", "tsfeatures", "Kats") %in% unique(data[[1]]$feature_set)) < 2){
    message("Only one set of 'catch22', 'feasts', 'tsfeatures', or 'Kats' with potential duplicates is in your feature data. Exiting and returning original input data.")
    return(data)
  } else{
    
    # Set up dictionary of duplicates and their concordance of names
    
    dictionary <- data.frame(feasts_name = c("n_crossing_points", "diff1_acf1", "diff1_acf10", "diff2_acf1", "diff2_acf10", "spectral_entropy", 
                                             "trend_strength", NA, NA, "coef_hurst", "stl_e_acf1", "stl_e_acf10", "acf1",
                                             "stat_arch_lm", "shift_kl_max", "shift_kl_index", "diff1_pacf5", "diff2_pacf5",
                                             "pacf5", "kpss_stat", NA),
                             tsfeatures_name = c("crossing_points", "diff1_acf1", "diff1_acf10", "diff2_acf1", "diff2_acf10", "entropy", 
                                                 "trend", "firstmin_ac", "firstzero_ac", "hurst", "e_acf1", "e_acf10", "x_acf1",
                                                 "ARCH.LM", "max_kl_shift", "time_kl_shift", "diff1x_pacf5", "diff2x_pacf5",
                                                 "x_pacf5", "unitroot_kpss", "fluctanal_prop_r1"),
                             Kats_name = c("crossing_points", "diff1y_acf1", NA, "diff2y_acf1", NA, NA, 
                                           NA, "firstmin_ac", "firstzero_ac", NA, NA, NA, "y_acf1",
                                           NA, NA, NA, NA, NA,
                                           NA, NA, NA),
                             catch22_name = c(NA, NA, NA, NA, NA, NA, NA, "CO_FirstMin_ac", NA, NA, NA, NA, NA, NA, NA,
                                              NA, NA, NA, NA, NA, "SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1"))
    
    rownames(dictionary) <- c("crossing_points", "diff1_acf1", "diff1_acf10", "diff2_acf1", "diff2_acf10", "entropy",
                              "trend", "firstmin_ac", "firstzero_ac", "hurst", "e_acf1", "e_acf10", "x_acf1", 
                              "ARCH.LM", "shift_kl_max", "time_kl_shift", "diff1_pacf5", "diff2x_pacf5", 
                              "x_pacf5", "kpss_stat", "SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1")
    
    # Set up features to remove based on sets in data
    
    sets_to_filter <- unique(data[[1]]$feature_set)[unique(data[[1]]$feature_set) %in% c("catch22", "feasts", "tsfeatures", "Kats")]
    other_sets_to_keep <- unique(data[[1]]$feature_set)[!unique(data[[1]]$feature_set) %in% sets_to_filter]
    
    if(sum(c("feasts", "tsfeatures", "Kats", "catch22") %in% unique(data[[1]]$feature_set)) == 3){
      sets_to_filter <- c("feasts", "tsfeatures", "Kats")
      dups <- dictionary
    } else if("feasts" %ni% unique(data[[1]]$feature_set) && sum(c("tsfeatures", "Kats") %in% unique(data[[1]]$feature_set) == 2)){
      sets_to_filter <- c("tsfeatures", "Kats")
      dups <- dictionary %>% dplyr::filter(!is.na(.data$tsfeatures_name)) %>% dplyr::filter(!is.na(.data$Kats_name)) %>% dplyr::select(c(.data$tsfeatures_name, .data$Kats_name))
    } else if("tsfeatures" %ni% unique(data[[1]]$feature_set) && sum(c("feasts", "Kats") %in% unique(data[[1]]$feature_set) == 2)){
      sets_to_filter <- c("feasts", "Kats")
      dups <- dictionary %>% dplyr::filter(!is.na(.data$feasts_name)) %>% dplyr::filter(!is.na(.data$Kats_name)) %>% dplyr::select(c(.data$feasts_name, .data$Kats_name))
    } else {
      sets_to_filter <- c("feasts", "tsfeatures")
      dups <- dictionary %>% dplyr::filter(!is.na(.data$feasts_name)) %>% dplyr::filter(!is.na(.data$tsfeatures_name)) %>% dplyr::select(c(.data$feasts_name, .data$tsfeatures_name))
    }
    
    # Retain other data
    
    other_sets <- data[[1]] %>%
      dplyr::filter(feature_set %in% other_sets_to_keep)
    
    # Handle duplicate features
    
    set.seed(seed)
    
    # Find which features to be handled exist in the data
    
    to_filter <- intersect(unique(as.vector(stats::na.omit(as.vector(t(as.matrix(dictionary)))))), unique(data[[1]]$names))
    to_filter_apply <- to_filter # To use later
    
    # Filter duplicate data
    
    dup_sets <- data[[1]] %>%
      dplyr::filter(feature_set %in% sets_to_filter) %>%
      dplyr::filter(names %in% to_filter)
    
    # Loop over vector of features to filter, dropping analogous names in the dictionary in each iteration to avoid double-ups or triple-ups of unique features
    
    contains_value <- function(row, my_vector) {
      any(sapply(row, function(col) col %in% my_vector))
    }
    
    storage <- list()
    
    while(length(to_filter_apply) > 0){
      
      # Filter dictionary
      
      tmp_dict <- dictionary %>%
        filter(apply(dictionary, 1, contains_value, to_filter_apply[1]))
      
      feats_to_exclude <- stats::na.omit(as.character(tmp_dict[1, ]))
      
      # Remove any analogous features from original vector so we don't double up
      
      to_filter_apply <- to_filter_apply[!to_filter_apply %in% feats_to_exclude]
      
      # Choose a set to retain
      
      feat <- tmp_dict[, colSums(is.na(tmp_dict)) == 0]
      colnames(feat) <- gsub("_name", "\\1", colnames(feat))
      selected_set <- sample(colnames(feat), size = 1, replace = FALSE)
      selected_feature <- as.character(feat[1, selected_set])
      
      # Filter duplicates from data
      
      tmp <- dup_sets %>%
        dplyr::filter(.data$names == selected_feature) %>%
        dplyr::mutate(keeper = dplyr::case_when(
          .data$feature_set == selected_set & .data$names == selected_feature ~ TRUE,
          TRUE                                                                ~ FALSE)) %>%
        dplyr::filter(.data$keeper) %>%
        dplyr::select(-c(.data$keeper))
      
      storage[[to_filter_apply[1]]] <- tmp
    }
    
    storage <- do.call("rbind", storage)
    
    # Add back in non-duplicate features from these sets
    
    dup_sets_other_feats <- data[[1]] %>%
      dplyr::filter(.data$feature_set %in% sets_to_filter) %>%
      dplyr::filter(.data$names %ni% unique(as.vector(stats::na.omit(as.vector(t(as.matrix(dictionary)))))))
    
    filtered_feats <- dplyr::bind_rows(storage, other_sets, dup_sets_other_feats)
    
    # Check we did not remove any more rows than we should have
    
    #stopifnot((length(na.omit(as.vector(t(as.matrix(dups))))) - nrow(dups)) * length(unique(data[[1]]$id)) == nrow(data[[1]]) - nrow(filtered_feats))
    
    # Return final object
    
    filtered_feats <- structure(list(filtered_feats), class = "feature_calculations")
    return(filtered_feats)
  }
}
