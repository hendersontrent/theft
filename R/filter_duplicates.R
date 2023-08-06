#' Helper function to remove duplicate features that exist in multiple feature sets
#' 
#' @import dplyr
#' 
#' @param data \code{feature_calculations} object containing the raw feature matrix produced by \code{calculate_features}
#' @param preference \code{character} denoting which feature set to keep (meaning the others will be filtered out) between \code{"feasts"}, \code{"tsfeatures"}, and \code{"Kats"} since there is considerable overlap between these three sets. Defaults to \code{"feasts"}. Duplicates will NOT be removed from sets when computing set-level results for the respective non-preferenced sets to ensure fairness. They are only filtered out for either the construction of the set of "All features" if \code{by_set = TRUE} and when computing individual feature results (to reduce redundant calculations)
#' @return \code{feature_calculations} object containing filtered feature data
#' @author Trent Henderson
#' @export
#' 

filter_duplicates <- function(data, preference = c("feasts", "tsfeatures", "Kats")){
  
  # Check preference
  
  preference <- match.arg(preference)
  '%ni%' <- Negate('%in%')
  
  if(preference %ni% c("feasts", "tsfeatures", "Kats")){
    stop("preference must be one of 'feasts', 'tsfeatures' or 'Kats'.")
  }
  
  sets_to_filter <- c("feasts", "tsfeatures", "Kats")[!c("feasts", "tsfeatures", "Kats") %in% preference]
  
  # Filter duplicates
  
  filtered_data <- data[[1]] %>%
    dplyr::mutate(flag = dplyr::case_when(
        .data$feature_set %in% sets_to_filter & .data$names %in% c("crossing_points", "n_crossing_points") ~ TRUE,
        .data$feature_set %in% sets_to_filter & .data$names == "curvature"                                 ~ TRUE,
        .data$feature_set %in% sets_to_filter & .data$names == "diff1_acf1"                                ~ TRUE,
        .data$feature_set %in% sets_to_filter & .data$names == "diff1_acf10"                               ~ TRUE,
        .data$feature_set %in% sets_to_filter & .data$names == "diff2_acf1"                                ~ TRUE,
        .data$feature_set %in% sets_to_filter & .data$names == "diff2_acf10"                               ~ TRUE,
        .data$feature_set %in% sets_to_filter & .data$names == "entropy"                                   ~ TRUE,
        .data$feature_set %in% sets_to_filter & .data$names == "flat_spots"                                ~ TRUE,
        .data$feature_set %in% sets_to_filter & .data$names == "histogram_mode"                            ~ TRUE,
        .data$feature_set %in% sets_to_filter & .data$names == "histogram_mode_10"                         ~ TRUE,
        .data$feature_set %in% sets_to_filter & .data$names == "linearity"                                 ~ TRUE,
        .data$feature_set %in% sets_to_filter & .data$names == "lumpiness"                                 ~ TRUE,
        .data$feature_set %in% sets_to_filter & .data$names == "spikiness"                                 ~ TRUE,
        .data$feature_set %in% sets_to_filter & .data$names == "stability"                                 ~ TRUE,
        .data$feature_set %in% sets_to_filter & .data$names == "std1st_der"                                ~ TRUE,
        .data$feature_set %in% sets_to_filter & .data$names == "trend_strength"                            ~ TRUE,
        .data$feature_set %in% sets_to_filter & .data$names == "unitroot_kpss"                             ~ TRUE,
        .data$feature_set %in% sets_to_filter & .data$names == "firstmin_ac"                               ~ TRUE,
        .data$feature_set %in% sets_to_filter & .data$names == "firstzero_ac"                              ~ TRUE,
        TRUE                                                                                               ~ FALSE)) %>%
    dplyr::filter(!.data$flag) %>%
    dplyr::select(-c(.data$flag))
  
  # Handle hurst separately since the name is slightly different between {feasts} and {tsfeatures}
  
  if(preference == "feasts"){
    filtered_data <- filtered_data %>%
      dplyr::mutate(flag = ifelse(.data$feature_set == "tsfeatures" & names == "hurst", TRUE, FALSE)) %>%
      dplyr::filter(!.data$flag) %>%
      dplyr::select(-c(.data$flag))
  } else if(preference == "tsfeatures"){
    filtered_data <- filtered_data %>%
      dplyr::mutate(flag = ifelse(.data$feature_set == "feasts" & names == "coef_hurst", TRUE, FALSE)) %>%
      dplyr::filter(!.data$flag) %>%
      dplyr::select(-c(.data$flag))
  } else{
    
  }
  
  filtered_data <- structure(list(filtered_data), class = "feature_calculations")
  return(filtered_data)
}
