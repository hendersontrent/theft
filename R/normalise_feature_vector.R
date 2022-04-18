#' Scale each value into a user-specified range for visualisation and analysis
#' @param x a vector of scalar values
#' @param method a rescaling/normalising method to apply. Defaults to \code{"RobustSigmoid"}
#' @return a vector of scalar values normalised into the selected range
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
#' x <- featMat[featMat$names == "DN_HistogramMode_5", ]
#' xnormed <- normalise_feature_vector(x$values, method = "RobustSigmoid")
#'

normalise_feature_vector <- function(x, method = c("z-score", "Sigmoid", "RobustSigmoid", "MinMax")){

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

  the_methods <- c("z-score", "Sigmoid", "RobustSigmoid", "MinMax")
  '%ni%' <- Negate('%in%')

  if(method %ni% the_methods){
    stop("method should be a single selection of 'z-score', 'Sigmoid', 'RobustSigmoid' or 'MinMax'")
  }

  if(length(method) > 1){
    stop("method should be a single selection of 'z-score', 'Sigmoid', 'RobustSigmoid' or 'MinMax'")
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

  return(x_norm)
}


#' Scale each value into a user-specified range for visualisation and analysis
#' @param x a vector of scalar values
#' @param method a rescaling/normalising method to apply. Defaults to \code{"RobustSigmoid"}
#' @return a vector of scalar values normalised into the selected range
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
#' x <- featMat[featMat$names == "DN_HistogramMode_5", ]
#' xnormed <- normalise_feature_vector(x$values, method = "RobustSigmoid")
#'

normalize_feature_vector <- function(x, method = c("z-score", "Sigmoid", "RobustSigmoid", "MinMax")){
  
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
  
  the_methods <- c("z-score", "Sigmoid", "RobustSigmoid", "MinMax")
  '%ni%' <- Negate('%in%')
  
  if(method %ni% the_methods){
    stop("method should be a single selection of 'z-score', 'Sigmoid', 'RobustSigmoid' or 'MinMax'")
  }
  
  if(length(method) > 1){
    stop("method should be a single selection of 'z-score', 'Sigmoid', 'RobustSigmoid' or 'MinMax'")
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
  
  return(x_norm)
}
