#' Scale each feature vector into a user-specified range for visualisation and modelling
#' @importFrom rlang .data
#' @import dplyr
#' @param data either a \code{feature_calculations} object containing the raw feature matrix produced by \code{calculate_features} or a \code{vector} of class \code{numeric} containing values to be normalised
#' @param method a rescaling/normalising method to apply to violin plots. Defaults to \code{"z-score"}
#' @return either an object of class \code{data.frame} or \code{numeric}
#' @author Trent Henderson
#' @export
#' 

normalise <- function(data, method = c("z-score", "Sigmoid", "RobustSigmoid", "MinMax")){
  
  method <- match.arg(method)
  
  if(inherits(data, "feature_calculations")){
    
    normed <- data[[1]] %>%
      dplyr::group_by(.data$names)
    
    if(method == "z-score"){
      normed <- normed %>%
        dplyr::mutate(values = zscore_scaler(.data$values))
    }
    
    if(method == "Sigmoid"){
      normed <- normed %>%
        dplyr::mutate(values = sigmoid_scaler(.data$values))
    }
    
    if(method == "RobustSigmoid"){
      normed <- normed %>%
        dplyr::mutate(values = robustsigmoid_scaler(.data$values))
    }
    
    if(method == "MinMax"){
      normed <- normed %>%
        dplyr::mutate(values = minmax_scaler(.data$values))
    }
    
    normed <- normed %>%
      dplyr::ungroup()
    
  } else{
    
    stopifnot(class(data) == "numeric")
    
    if(method == "z-score"){
      normed <- zscore_scaler(data)
    }
    
    if(method == "Sigmoid"){
      normed <- sigmoid_scaler(data)
    }
    
    if(method == "RobustSigmoid"){
      normed <- robustsigmoid_scaler(data)
    }
    
    if(method == "MinMax"){
      normed <- minmax_scaler(data)
    }
  }
  
  return(normed)
}
