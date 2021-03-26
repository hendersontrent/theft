#------------------- Helper functions to reduce length -------------

calc_feasts <- function(data){
  
  tmp_dat <- list(data)
  
  # Calculate features
  
  outData <- features(data)
  
  # Wrangle dataframe into format consistent with {catchEmAll}
  
  outData <- outData
  
  return(outData)
}

calc_tsfeatures <- function(data){
  
  tmp_dat <- list(data)
  
  # Calculate features
  
  outData <- tsfeatures::tsfeatures(data)
  
  # Wrangle dataframe into format consistent with {catchEmAll}
  
  outData <- outData
  
  return(outData)
}

#------------------- Main exported calculation function ------------

#' Automatically run time-series feature calculations included in the package
#' @import dplyr
#' @import catchEmAll
#' @import feasts
#' @import tsfeatures
#' @param data a numerical time-series input vector
#' @param feature_set The set of time-series features to calculate. Defaults to 'all'
#' @return object of class DataFrame that contains the summary statistics for each feature
#' @author Trent Henderson
#' @export
#' @examples
#' data <- 1 + 0.5 * 1:1000 + arima.sim(list(ma = 0.5), n = 1000)
#' outs <- calculate_features(data, feature_set = "all")
#'

calculate_features <- function(data, feature_set = c("all", "catchEmAll", "catch22", "catchaMouse16", "feasts", "tsfeatures")){
  
  # Make 'all' the default
  
  if(missing(feature_set)){
    feature_set <- "all"
  } else{
    feature_set <- match.arg(feature_set)
  }
  
  #--------- Error catches ---------
  
  # Input vector
  
  if(!is.numeric(data)){
    stop("data should be a vector of numeric values.")
  }
  
  if(length(data) < 5){
    stop("length of data is too short to make reliable calculations.")
  }
  
  # Method selection
  
  the_sets <- c("all", "catchEmAll", "catch22", "catchaMouse16", "feasts", "tsfeatures")
  '%ni%' <- Negate('%in%')
  
  if(feature_set %ni% the_sets){
    stop("feature_set should be a selection or combination of 'all', 'catchEmAll', 'catch22', 'catchaMouse16', 'feasts' or 'tsfeatures' entered as a single string or vector for multiple.")
  }
  
  #--------- Feature calcs --------
  
  if(feature_set == "all"){
    
    tmp <- catchEmAll::catch22_all(data)
    tmp1 <- catchEmAll::catchaMouse16_all(data)
    tmp2 <- calc_feasts(data)
    tmp3 <- calc_tsfeatures(data)
    
    tmp_all <- dplyr::bind_rows(tmp, tmp1, tmp2, tmp3)
  }
  
  if(feature_set == "catchEmAll"){
    
    tmp <- catchEmAll::catch22_all(data)
    tmp1 <- catchEmAll::catchaMouse16_all(data)
    
    tmp_all <- dplyr::bind_rows(tmp, tmp1)
  }
  
  if(feature_set == "catch22"){
    
    tmp_all <- catchEmAll::catch22_all(data)
  }
  
  if(feature_set == "catchaMouse16"){
    
    tmp_all <- catchEmAll::catchaMouse16_all(data)
  }
  
  if(feature_set == "feasts"){
    
    tmp_all <- calc_feasts(data)
  }
  
  if(feature_set == "tsfeatures"){
    
    tmp_all <- calc_tsfeatures(data)
  }
  
  return(tmp_all)
}
