#------------------- Helper functions ------------------------------

zscore_function <- function(data){
  
  tmp <- data %>%
    dplyr::group_by(.data$names) %>%
    dplyr::mutate(values = normalise_feature_vector(.data$values, method = "z-score")) %>%
    dplyr::ungroup()
  
  return(tmp)
}

sigmoid_function <- function(data){
  
  tmp <- data %>%
    dplyr::group_by(.data$names) %>%
    dplyr::mutate(values = normalise_feature_vector(.data$values, method = "Sigmoid")) %>%
    dplyr::ungroup()
  
  return(tmp)
}

rsigmoid_function <- function(data){
  
  tmp <- data %>%
    dplyr::group_by(.data$names) %>%
    dplyr::mutate(values = normalise_feature_vector(.data$values, method = "RobustSigmoid")) %>%
    dplyr::ungroup()
  
  return(tmp)
}

mm_function <- function(data){
  
  tmp <- data %>%
    dplyr::group_by(.data$names) %>%
    dplyr::mutate(values = normalise_feature_vector(.data$values, method = "MinMax")) %>%
    dplyr::ungroup()
  
  return(tmp)
}

#------------------- Main function ---------------------------------

#' Scale each feature vector into a user-specified range for visualisation and modelling
#' @importFrom rlang .data
#' @import dplyr
#' @param data a dataframe with at least 2 columns: names variable (feature names) and value variable
#' @param names_var a string denoting the name of the variable/column that holds the feature names. Defaults to \code{"names"}
#' @param values_var a string denoting the name of the variable/column that holds the numerical feature values. Defaults to \code{"values"}
#' @param method a rescaling/normalising method to apply. Defaults to \code{"RobustSigmoid"}
#' @return a dataframe with the value column rescaled into the specified range
#' @author Trent Henderson
#' @export
#' @examples
#' \donttest{
#' featMat <- calculate_features(data = simData, 
#'   id_var = "id", 
#'   time_var = "timepoint", 
#'   values_var = "values", 
#'   group_var = "process", 
#'   feature_set = "catch22")
#'   
#' normed <- normalise_feature_frame(featMat, 
#'   names_var = "names", 
#'   values_var = "values", 
#'   method = "RobustSigmoid")
#' }
#'

normalise_feature_frame <- function(data, names_var = "names", values_var = "values", 
                                    method = c("z-score", "Sigmoid", "RobustSigmoid", "MinMax")){
  
  # Make RobustSigmoid the default
  
  if(missing(method)){
    method <- "RobustSigmoid"
  }
  
  if(is.null(method)){
    method <- "RobustSigmoid"
  }
  
  #--------- Error catches ---------
  
  # Method selection
  
  the_methods <- c("z-score", "Sigmoid", "RobustSigmoid", "MinMax")
  '%ni%' <- Negate('%in%')
  
  if(method %ni% the_methods){
    stop("method should be a single selection of 'z-score', 'Sigmoid', 'RobustSigmoid' or 'MinMax'")
  }
  
  if(length(method) > 1){
    stop("method should be a single selection of 'z-score', 'Sigmoid', 'RobustSigmoid' or 'MinMax'")
  }
  
  # Variables
  
  if(is.null(names_var) || is.null(values_var)){
    stop("Input dataframe must have at least 2 columns representing variables for: names (features) and values.")
  }
  
  #--------- Apply scaling ---------
  
  data_re <- data %>%
    dplyr::rename(names = dplyr::all_of(names_var),
                  values = dplyr::all_of(values_var))
  
  if(method == "z-score"){
    tmp <- zscore_function(data = data)
  }
  
  if(method == "Sigmoid"){
    tmp <- sigmoid_function(data = data)
  }
  
  if(method == "RobustSigmoid"){
    tmp <- rsigmoid_function(data = data)
  }
  
  if(method == "MinMax"){
    tmp <- mm_function(data = data)
  }
  
  return(tmp)
}


#' Scale each feature vector into a user-specified range for visualisation and modelling
#' @importFrom rlang .data
#' @import dplyr
#' @param data a dataframe with at least 2 columns: names variable (feature names) and value variable
#' @param names_var a string denoting the name of the variable/column that holds the feature names. Defaults to \code{"names"}
#' @param values_var a string denoting the name of the variable/column that holds the numerical feature values. Defaults to \code{"values"}
#' @param method a rescaling/normalising method to apply. Defaults to \code{"RobustSigmoid"}
#' @return a dataframe with the value column rescaled into the specified range
#' @author Trent Henderson
#' @export
#' @examples
#' \donttest{
#' featMat <- calculate_features(data = simData, 
#'   id_var = "id", 
#'   time_var = "timepoint", 
#'   values_var = "values", 
#'   group_var = "process", 
#'   feature_set = "catch22")
#'   
#' normed <- normalize_feature_frame(featMat, 
#'   names_var = "names", 
#'   values_var = "values", 
#'   method = "RobustSigmoid")
#' }
#'

normalize_feature_frame <- function(data, names_var = "names", values_var = "values", 
                                    method = c("z-score", "Sigmoid", "RobustSigmoid", "MinMax")){
  
  # Make RobustSigmoid the default
  
  if(missing(method)){
    method <- "RobustSigmoid"
  }
  
  if(is.null(method)){
    method <- "RobustSigmoid"
  }
  
  #--------- Error catches ---------
  
  # Method selection
  
  the_methods <- c("z-score", "Sigmoid", "RobustSigmoid", "MinMax")
  '%ni%' <- Negate('%in%')
  
  if(method %ni% the_methods){
    stop("method should be a single selection of 'z-score', 'Sigmoid', 'RobustSigmoid' or 'MinMax'")
  }
  
  if(length(method) > 1){
    stop("method should be a single selection of 'z-score', 'Sigmoid', 'RobustSigmoid' or 'MinMax'")
  }
  
  # Variables
  
  if(is.null(names_var) || is.null(values_var)){
    stop("Input dataframe must have at least 2 columns representing variables for: names (features) and values.")
  }
  
  #--------- Apply scaling ---------
  
  data_re <- data %>%
    dplyr::rename(names = dplyr::all_of(names_var),
                  values = dplyr::all_of(values_var))
  
  if(method == "z-score"){
    tmp <- zscore_function(data = data)
  }
  
  if(method == "Sigmoid"){
    tmp <- sigmoid_function(data = data)
  }
  
  if(method == "RobustSigmoid"){
    tmp <- rsigmoid_function(data = data)
  }
  
  if(method == "MinMax"){
    tmp <- mm_function(data = data)
  }
  
  return(tmp)
}
