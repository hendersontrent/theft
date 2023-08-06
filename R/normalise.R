#' Scale each feature vector into a user-specified range for visualisation and modelling
#' 
#' @importFrom rlang .data
#' @importFrom scales rescale
#' @import dplyr
#' @param data either a \code{feature_calculations} object containing the raw feature matrix produced by \code{calculate_features} or a \code{vector} of class \code{numeric} containing values to be normalised
#' @param norm_method \code{character} denoting the rescaling/normalising method to apply to violin plots. Defaults to \code{"z-score"}
#' @param unit_int \code{Boolean} whether to rescale into unit interval \code{[0,1]} after applying normalisation method. Defaults to \code{FALSE}
#' @return either an object of class \code{data.frame} or \code{numeric}
#' @author Trent Henderson
#' @export
#' 

normalise <- function(data, norm_method = c("z-score", "Sigmoid", "RobustSigmoid", "MinMax"), unit_int = FALSE){
  
  norm_method <- match.arg(norm_method)
  
  if(inherits(data, "feature_calculations")){
    
    normed <- data[[1]] %>%
      dplyr::group_by(.data$names)
    
    if(norm_method == "z-score"){
      normed <- normed %>%
        dplyr::mutate(values = zscore_scaler(.data$values))
    }
    
    if(norm_method == "Sigmoid"){
      normed <- normed %>%
        dplyr::mutate(values = sigmoid_scaler(.data$values))
    }
    
    if(norm_method == "RobustSigmoid"){
      normed <- normed %>%
        dplyr::mutate(values = robustsigmoid_scaler(.data$values))
    }
    
    if(norm_method == "MinMax"){
      normed <- normed %>%
        dplyr::mutate(values = minmax_scaler(.data$values))
    }
    
    if(unit_int){
      normed <- normed %>%
        dplyr::mutate(values = scales::rescale(.data$values, to = c(0, 1)))
    }
    
    normed <- normed %>%
      dplyr::ungroup()
    
  } else{
    
    stopifnot(class(data) == "numeric")
    
    if(norm_method == "z-score"){
      normed <- zscore_scaler(data)
    }
    
    if(norm_method == "Sigmoid"){
      normed <- sigmoid_scaler(data)
    }
    
    if(norm_method == "RobustSigmoid"){
      normed <- robustsigmoid_scaler(data)
    }
    
    if(norm_method == "MinMax"){
      normed <- minmax_scaler(data)
    }
    
    if(unit_int){
      normed <- scales::rescale(normed, to = c(0, 1))
    }
  }
  
  return(normed)
}
