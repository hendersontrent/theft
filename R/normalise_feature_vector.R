#' Scale each value into a user-specified range for visualisation and analysis
#' @param x a vector of scalar values
#' @param method a rescaling/normalising method to apply. Defaults to 'RobustSigmoid'
#' @return a vector of scalar values normalised into the selected range
#' @author Trent Henderson
#' @export
#' @examples
#' library(dplyr)
#' library(tsibbledata)
#' 
#' d <- tsibbledata::aus_retail %>%
#'   rename(Series_ID = 3)
#' 
#' feature_matrix <- calculate_features(data = d, 
#'   id_var = "Series_ID", 
#'   time_var = "Month", 
#'   values_var = "Turnover", 
#'   group_var = "State",
#'   feature_set = "catch22")
#'   
#' x <- feature_matrix %>%
#'   filter(names == "DN_HistogramMode_5") %>%
#'   pull(values)
#'   
#' xnormed <- normalise_feature_vector(x, method = "MinMax")
#'

normalise_feature_vector <- function(x, method = c("z-score", "Sigmoid", "RobustSigmoid", "MinMax", "MeanSubtract")){

  # Make RobustSigmoid the default

  if(missing(method)){
    method <- "RobustSigmoid"
  } else{
    method <- match.arg(method)
  }

  #--------- Error catches ---------

  # Input vector

  if(!is.numeric(x)){
    stop("x should be a vector of numeric values.")
  }

  # Method selection

  the_methods <- c("z-score", "Sigmoid", "RobustSigmoid", "MinMax", "MeanSubtract")
  '%ni%' <- Negate('%in%')

  if(method %ni% the_methods){
    stop("method should be a single selection of 'z-score', 'Sigmoid', 'RobustSigmoid', 'MinMax' or 'MeanSubtract'")
  }

  if(length(method) > 1){
    stop("method should be a single selection of 'z-score', 'Sigmoid', 'RobustSigmoid', 'MinMax' or 'MeanSubtract'")
  }

  #--------- Apply scaling ---------

  if(method == "z-score"){
    x_norm <- zscore_scaler(x)
  }

  if(method == "Sigmoid"){
    x_norm <- sigmoid_scaler(x)
  }

  if(method == "RobustSigmoid"){
    x_norm <- robustsigmoid_scaler(x)
  }

  if(method == "MinMax"){
    x_norm <- minmax_scaler(x)
  }

  if(method == "MeanSubtract"){
    x_norm <- mean_scaler(x)
  }

  return(x_norm)
}


#' Scale each value into a user-specified range for visualisation and analysis
#' @param x a vector of scalar values
#' @param method a rescaling/normalising method to apply. Defaults to 'RobustSigmoid'
#' @return a vector of scalar values normalised into the selected range
#' @author Trent Henderson
#' @export
#' @examples
#' library(dplyr)
#' library(tsibbledata)
#' 
#' d <- tsibbledata::aus_retail %>%
#'   rename(Series_ID = 3)
#' 
#' feature_matrix <- calculate_features(data = d, 
#'   id_var = "Series_ID", 
#'   time_var = "Month", 
#'   values_var = "Turnover", 
#'   group_var = "State",
#'   feature_set = "catch22")
#'   
#' x <- feature_matrix %>%
#'   filter(names == "DN_HistogramMode_5") %>%
#'   pull(values)
#'   
#' xnormed <- normalise_feature_vector(x, method = "MinMax")
#'

normalize_feature_vector <- function(x, method = c("z-score", "Sigmoid", "RobustSigmoid", "MinMax", "MeanSubtract")){
  
  # Make RobustSigmoid the default
  
  if(missing(method)){
    method <- "RobustSigmoid"
  } else{
    method <- match.arg(method)
  }
  
  #--------- Error catches ---------
  
  # Input vector
  
  if(!is.numeric(x)){
    stop("x should be a vector of numeric values.")
  }
  
  # Method selection
  
  the_methods <- c("z-score", "Sigmoid", "RobustSigmoid", "MinMax", "MeanSubtract")
  '%ni%' <- Negate('%in%')
  
  if(method %ni% the_methods){
    stop("method should be a single selection of 'z-score', 'Sigmoid', 'RobustSigmoid', 'MinMax' or 'MeanSubtract'")
  }
  
  if(length(method) > 1){
    stop("method should be a single selection of 'z-score', 'Sigmoid', 'RobustSigmoid', 'MinMax' or 'MeanSubtract'")
  }
  
  #--------- Apply scaling ---------
  
  if(method == "z-score"){
    x_norm <- zscore_scaler(x)
  }
  
  if(method == "Sigmoid"){
    x_norm <- sigmoid_scaler(x)
  }
  
  if(method == "RobustSigmoid"){
    x_norm <- robustsigmoid_scaler(x)
  }
  
  if(method == "MinMax"){
    x_norm <- minmax_scaler(x)
  }
  
  if(method == "MeanSubtract"){
    x_norm <- mean_scaler(x)
  }
  
  return(x_norm)
}
