#------------------- Helper functions ------------------------------

zscore_function <- function(data, names = NULL, values = NULL){
  
  tmp <- data %>%
    dplyr::group_by(names) %>%
    dplyr::mutate(values = catch22::normalise_catch(values, method = "z-score")) %>%
    dplyr::ungroup()
  
  return(tmp)
}

sigmoid_function <- function(data, names = NULL, values = NULL){
  
  tmp <- data %>%
    dplyr::group_by(names) %>%
    dplyr::mutate(values = catch22::normalise_catch(values, method = "Sigmoid")) %>%
    dplyr::ungroup()
  
  return(tmp)
}

rsigmoid_function <- function(data, names = NULL, values = NULL){
  
  tmp <- data %>%
    dplyr::group_by(names) %>%
    dplyr::mutate(values = catch22::normalise_catch(values, method = "RobustSigmoid")) %>%
    dplyr::ungroup()
  
  return(tmp)
}

mm_function <- function(data, names = NULL, values = NULL){
  
  tmp <- data %>%
    dplyr::group_by(names) %>%
    dplyr::mutate(values = catch22::normalise_catch(values, method = "MinMax")) %>%
    dplyr::ungroup()
  
  return(tmp)
}

ms_function <- function(data, names = NULL, values = NULL){
  
  tmp <- data %>%
    dplyr::group_by(names) %>%
    dplyr::mutate(values = catch22::normalise_catch(values, method = "MeanSubtract")) %>%
    dplyr::ungroup()
  
  return(tmp)
}

#------------------- Main function ---------------------------------

#' Scale each feature vector into a user-specified range for visualisation and modelling
#' @import dplyr
#' @param data a dataframe with at least 2 columns: names variable (feature names) and value variable
#' @param names_var a string denoting the name of the variable/column that holds the feature names
#' @param values_var a string denoting the name of the variable/column that holds the numerical feature values
#' @param method a rescaling/normalising method to apply. Defaults to 'RobustSigmoid'
#' @return a dataframe with the value column rescaled into the specified range
#' @author Trent Henderson
#' @export
#' @examples
#' \dontrun{
#' outs <- sawlog::calculate_features(tsibbledata::ausretail, id_var = "Industry", group_var = "State", time_var = "Month", value_var = "Turnover", feature_set = "feasts")
#' outsNormed <- normalise_feature_frame(outs, names_var = "names", values_var = "values", method = "RobustSigmoid")
#'}

normalise_feature_frame <- function(data, names_var = NULL, values_var = NULL, method = c("z-score", "Sigmoid", "RobustSigmoid", "MinMax", "MeanSubtract")){
  
  # Make RobustSigmoid the default
  
  if(missing(method)){
    method <- "RobustSigmoid"
  }
  
  if(is.null(method)){
    method <- "RobustSigmoid"
  }
  
  #--------- Error catches ---------
  
  # Method selection
  
  the_methods <- c("z-score", "Sigmoid", "RobustSigmoid", "MinMax", "MeanSubtract")
  '%ni%' <- Negate('%in%')
  
  if(method %ni% the_methods){
    stop("method should be a single selection of 'z-score', 'Sigmoid', 'RobustSigmoid', 'MinMax' or 'MeanSubtract'")
  }
  
  if(length(method) > 1){
    stop("method should be a single selection of 'z-score', 'Sigmoid', 'RobustSigmoid', 'MinMax' or 'MeanSubtract'")
  }
  
  # Variables
  
  if(is.null(names_var) | is.null(values_var)){
    stop("Input dataframe must have at least 2 columns representing variables for: names (features) and values.")
  }
  
  #--------- Apply scaling ---------
  
  if(method == "z-score"){
    tmp <- zscore_function(data = data, names = names_var, values = values_var)
  }
  
  if(method == "Sigmoid"){
    tmp <- sigmoid_function(data = data, names = names_var, values = values_var)
  }
  
  if(method == "RobustSigmoid"){
    tmp <- rsigmoid_function(data = data, names = names_var, values = values_var)
  }
  
  if(method == "MinMax"){
    tmp <- mm_function(data = data, names = names_var, values = values_var)
  }
  
  if(method == "MeanSubtract"){
    tmp <- ms_function(data = data, names = names_var, values = values_var)
  }
  
  return(tmp)
}



#' Scale each feature vector into a user-specified range for visualisation and modelling
#' @import dplyr
#' @param data a dataframe with at least 2 columns: names variable (feature names) and value variable
#' @param names_var a string denoting the name of the variable/column that holds the feature names
#' @param values_var a string denoting the name of the variable/column that holds the numerical feature values
#' @param method a rescaling/normalising method to apply. Defaults to 'RobustSigmoid'
#' @return a dataframe with the value column rescaled into the specified range
#' @author Trent Henderson
#' @export
#' @examples
#' \dontrun{
#' outs <- sawlog::calculate_features(tsibbledata::ausretail, id_var = "Industry", group_var = "State", time_var = "Month", value_var = "Turnover", feature_set = "feasts")
#' outsNormed <- normalize_feature_frame(outs, names_var = "names", values_var = "values", method = "RobustSigmoid")
#'}

normalize_feature_frame <- function(data, names_var = NULL, values_var = NULL, method = c("z-score", "Sigmoid", "RobustSigmoid", "MinMax", "MeanSubtract")){
  
  # Make RobustSigmoid the default
  
  if(missing(method)){
    method <- "RobustSigmoid"
  }
  
  if(is.null(method)){
    method <- "RobustSigmoid"
  }
  
  #--------- Error catches ---------
  
  # Method selection
  
  the_methods <- c("z-score", "Sigmoid", "RobustSigmoid", "MinMax", "MeanSubtract")
  '%ni%' <- Negate('%in%')
  
  if(method %ni% the_methods){
    stop("method should be a single selection of 'z-score', 'Sigmoid', 'RobustSigmoid', 'MinMax' or 'MeanSubtract'")
  }
  
  if(length(method) > 1){
    stop("method should be a single selection of 'z-score', 'Sigmoid', 'RobustSigmoid', 'MinMax' or 'MeanSubtract'")
  }
  
  # Variables
  
  if(is.null(names_var) | is.null(values_var)){
    stop("Input dataframe must have at least 2 columns representing variables for: names (features) and values.")
  }
  
  #--------- Apply scaling ---------
  
  if(method == "z-score"){
    tmp <- zscore_function(data = data, names = names_var, values = values_var)
  }
  
  if(method == "Sigmoid"){
    tmp <- sigmoid_function(data = data, names = names_var, values = values_var)
  }
  
  if(method == "RobustSigmoid"){
    tmp <- rsigmoid_function(data = data, names = names_var, values = values_var)
  }
  
  if(method == "MinMax"){
    tmp <- mm_function(data = data, names = names_var, values = values_var)
  }
  
  if(method == "MeanSubtract"){
    tmp <- ms_function(data = data, names = names_var, values = values_var)
  }
  
  return(tmp)
}
