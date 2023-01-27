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
    
    normed <- data %>%
      dplyr::rename(names = dplyr::all_of(names_var),
                    values = dplyr::all_of(values_var)) %>%
      dplyr::group_by(.data$names) %>%
      dplyr::mutate(values = normalise_feature_vector(.data$values, method = method)) %>%
      dplyr::ungroup()
    
  } else{
    if(method == "z-score"){
      normed <- zscore_scaler(x)
    }
    
    if(method == "Sigmoid"){
      normed <- sigmoid_scaler(x)
    }
    
    if(method == "RobustSigmoid"){
      normed <- robustsigmoid_scaler(x)
    }
    
    if(method == "MinMax"){
      normed <- minmax_scaler(x)
    }
  }
  
  return(normed)
}
