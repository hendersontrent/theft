#' Helper function to remove duplicate features that exist in multiple feature sets
#' 
#' @import dplyr
#' 
#' @param data \code{feature_calculations} object containing the raw feature matrix produced by \code{calculate_features}
#' @param preference \code{character} denoting which feature set to keep (meaning the others will be filtered out) between \code{"feasts"}, \code{"tsfeatures"}, and \code{"Kats"} since there is considerable overlap between these three sets. Defaults to \code{"feasts"}. Only applies if \code{by_set = TRUE} (since a set of "All features" is constructed automatically as a comparator)
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
        .data$method %in% sets_to_filter & .data$names %in% c("crossing_points", "n_crossing_points") ~ TRUE,
        .data$method %in% sets_to_filter & .data$names == "curvature"                                 ~ TRUE,
        .data$method %in% sets_to_filter & .data$names == "diff1_acf1"                                ~ TRUE,
        .data$method %in% sets_to_filter & .data$names == "diff1_acf10"                               ~ TRUE,
        .data$method %in% sets_to_filter & .data$names == "diff2_acf1"                                ~ TRUE,
        .data$method %in% sets_to_filter & .data$names == "diff2_acf10"                               ~ TRUE,
        .data$method %in% sets_to_filter & .data$names == "entropy"                                   ~ TRUE,
        .data$method %in% sets_to_filter & .data$names == "flat_spots"                                ~ TRUE,
        .data$method %in% sets_to_filter & .data$names == "histogram_mode"                            ~ TRUE,
        .data$method %in% sets_to_filter & .data$names == "histogram_mode_10"                         ~ TRUE,
        .data$method %in% sets_to_filter & .data$names == "linearity"                                 ~ TRUE,
        .data$method %in% sets_to_filter & .data$names == "lumpiness"                                 ~ TRUE,
        .data$method %in% sets_to_filter & .data$names == "spikiness"                                 ~ TRUE,
        .data$method %in% sets_to_filter & .data$names == "stability"                                 ~ TRUE,
        .data$method %in% sets_to_filter & .data$names == "std1st_der"                                ~ TRUE,
        .data$method %in% sets_to_filter & .data$names == "trend_strength"                            ~ TRUE,
        .data$method %in% sets_to_filter & .data$names == "unitroot_kpss"                             ~ TRUE,
        .data$method %in% sets_to_filter & .data$names == "firstmin_ac"                               ~ TRUE,
        .data$method %in% sets_to_filter & .data$names == "firstzero_ac"                              ~ TRUE,
        TRUE                                                                                          ~ FALSE)) %>%
    dplyr::filter(!flag) %>%
    dplyr::select(-c(flag))
  
  # Handle hurst separately since the name is slightly different between {feasts} and {tsfeatures}
  
  if(preference == "feasts"){
    filtered_data <- filtered_data %>%
      dplyr::mutate(flag = ifelse(.data$method == "tsfeatures" & names == "hurst", TRUE, FALSE)) %>%
      dplyr::filter(!flag) %>%
      dplyr::select(-c(flag))
  } else if(preference == "tsfeatures"){
    filtered_data <- filtered_data %>%
      dplyr::mutate(flag = ifelse(.data$method == "feasts" & names == "coef_hurst", TRUE, FALSE)) %>%
      dplyr::filter(!flag) %>%
      dplyr::select(-c(flag))
  } else{
    
  }
  
  filtered_data <- structure(list(filtered_data), class = "feature_calculations")
  return(filtered_data)
}
