#' Wrapper function to compute features on an input time series dataset
#' 
#' @inheritParams purloiner::extract_features
#' @return object of class \code{feature_calculations} that contains the summary statistics for each feature
#' @author Trent Henderson
#' @export
#' @examples
#' featMat <- calculate_features(data = simData, 
#'   id_var = "id", 
#'   time_var = "timepoint", 
#'   values_var = "values", 
#'   group_var = "process", 
#'   feature_set = "catch22",
#'   seed = 123)
#'

calculate_features <- purloiner::extract_features
