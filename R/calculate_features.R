#------------------- Helper functions to reduce length -------------

# catch22

calc_catch22 <- function(data, id, group, time, value){
  
  storage <- list()
  ids <- unique(data$id)
  
  for(i in ids){
    
    message(paste0("Calculating features for ID: ",i))
    
    tsPrep <- data %>%
      dplyr::filter(id == i) %>%
      dplyr::arrange(timepoint)
    
    tsData <- tsPrep$value
    
    # Feature calcs
    
    tmp <- catchEmAll::catch22_all(tsData) %>%
      dplyr::mutate(id = i,
                    method = "catch22")
    
    storage[[i]] <- tmp
  }
  
  # Pull into one tidy dataframe
  
  outData <- data.table::rbindlist(storage, use.names = TRUE)
  
  return(outData)
}

# catchaMouse16

calc_catchaMouse16 <- function(data, id, group, time, value){
  
  storage <- list()
  ids <- unique(data$id)
  
  for(i in ids){
    
    message(paste0("Calculating features for ID: ",i))
    
    tsPrep <- data %>%
      dplyr::filter(id == i) %>%
      dplyr::arrange(timepoint)
    
    tsData <- tsPrep$value
    
    # Feature calcs
    
    tmp <- catchEmAll::catchaMouse16_all(tsData) %>%
      dplyr::mutate(id = i,
                    method = "catchaMouse16")
    
    storage[[i]] <- tmp
  }
  
  # Pull into one tidy dataframe
  
  outData <- data.table::rbindlist(storage, use.names = TRUE)
  
  return(outData)
}

# feasts

calc_feasts <- function(data, id, group, time, value){
  
  storage <- list()
  ids <- unique(data$id)
  
  for(i in ids){
    
    message(paste0("Calculating features for ID: ",i))
    
    tsPrep <- data %>%
      dplyr::filter(id == i) %>%
      dplyr::arrange(time)
    
    tsData <- tsibble::as_tsibble(tsPrep, key = id, index = time)
    
    # Feature calcs
    
    tmp <- tsData %>%
      fabletools::features(value, fabletools::feature_set(pkgs = "feasts")) %>%
      dplyr::mutate(id = id) %>%
      tidyr::pivot_longer(!id, names_to = "names", values_to = "values")
    
    storage[[i]] <- tmp
  }
  
  # Pull into one tidy dataframe
  
  outData <- data.table::rbindlist(storage, use.names = TRUE)
  
  return(outData)
}

# tsfeatures

calc_tsfeatures <- function(data, id, group, time, value){
  
  storage <- list()
  ids <- unique(data$id)
  
  for(i in ids){
    
    message(paste0("Calculating features for ID: ",i))
    
    tsPrep <- data %>%
      dplyr::filter(id == i) %>%
      dplyr::arrange(time)
    
    tsData <- list(tsPrep$value)
    
    # Feature calcs
    
    tmp <- tsfeatures::tsfeatures(tsData) %>%
      dplyr::mutate(id = i) %>%
      tidyr::pivot_longer(cols = !id, names_to = "names", values_to = "values") %>%
      dplyr::mutate(method = "tsfeatures")
    
    storage[[i]] <- tmp
  }
  
  # Pull into one tidy dataframe
  
  outData <- data.table::rbindlist(storage, use.names = TRUE)
  
  return(outData)
}

#------------------- Main exported calculation function ------------

#' Automatically run time-series feature calculations included in the package
#' @import dplyr
#' @import catchEmAll
#' @import feasts
#' @import tsfeatures
#' @import tsibble
#' @importFrom tidyr pivot_longer
#' @importFrom data.table rbindlist
#' @importFrom fabletools features
#' @importFrom fabletools feature_set
#' @param data a dataframe with at least 4 columns: id variable, group variable, time variable, value variable
#' @param id_var a string specifying the ID variable to group data on (if one exists). Defaults to NULL
#' @param group_var a string specifying the grouping variable that the data aggregates to. Defaults to NULL
#' @param feature_set The set of time-series features to calculate. Defaults to 'all'
#' @return object of class DataFrame that contains the summary statistics for each feature
#' @author Trent Henderson
#' @export
#' @examples
#' \dontrun{
#' library(dplyr)
#' d <- data.frame(value = 1 + 0.5 * 1:1000 + arima.sim(list(ma = 0.5), n = 1000)) %>%
#' dplyr::mutate(id = 1,
#'               group = 1,
#'               timepoint = row_number())
#' outs <- calculate_features(data = d, id_var = "id", group_var = "group", time_var = "timepoint", value_var = "value", feature_set = "all")
#' }
#'

calculate_features <- function(data, id_var = NULL, group_var = NULL, time_var = NULL, value_var = NULL,
                               feature_set = c("all", "catch22", "catchaMouse16", "feasts", "tsfeatures")){
  
  if(is.null(id_var) | is.null(group_var) | is.null(time_var) | is.null(value_var)){
    stop("As {tsibble} currently cannot handle numeric vectors, input must be a dataframe with at least 4 columns: id, group, timepoint, value")
  }
  
  # Make 'all' the default
  
  if(missing(feature_set)){
    feature_set <- "all"
  }
  
  if(is.null(feature_set)){
    feature_set <- "all"
  }
  
  #--------- Error catches ---------
  
  # Method selection
  
  the_sets <- c("all", "catch22", "catchaMouse16", "feasts", "tsfeatures")
  '%ni%' <- Negate('%in%')
  
  if(feature_set %ni% the_sets){
    stop("feature_set should be a selection or combination of 'all', 'catch22', 'catchaMouse16', 'feasts' or 'tsfeatures' entered as a single string or vector for multiple.")
  }
  
  #--------- Feature calcs --------
  
  message("Calculating features... This may take a long time to complete depending on the size of your data and the number of features selected.")
  
  if("all" %in% feature_set){
    
    tmp <- calc_catch22(data = data, id = id_var, group = group_var, time = time_var, value = value_var)
    tmp1 <- calc_catchaMouse16(data = data, id = id_var, group = group_var, time = time_var, value = value_var)
    tmp2 <- calc_feasts(data = data, id = id_var, group = group_var, time = time_var, value = value_var)
    tmp3 <- calc_tsfeatures(data = data, id = id_var, group = group_var, time = time_var, value = value_var)
    
    tmp_all <- dplyr::bind_rows(tmp, tmp1, tmp2, tmp3)
  }
  
  if("catch22" %in% feature_set){
    
    tmp <- calc_catch22(data = data, id = id_var, group = group_var, time = time_var, value = value_var)
  }
  
  if("catchaMouse16" %in% feature_set){
    
    tmp1 <- calc_catchaMouse16(data = data, id = id_var, group = group_var, time = time_var, value = value_var)
  }
  
  if("feasts" %in% feature_set){
    
    tmp2 <- calc_feasts(data = data, id = id_var, group = group_var, time = time_var, value = value_var)
  }
  
  if("tsfeatures" %in% feature_set){
    
    tmp3 <- calc_tsfeatures(data = data, id = id_var, group = group_var, time = time_var, value = value_var)
  }
  
  tmp_all <- data.frame()
  
  if(exists("tmp")){
    tmp_all <- dplyr::bind_rows(tmp_all, tmp)
  }
  
  if(exists("tmp1")){
    tmp_all <- dplyr::bind_rows(tmp_all, tmp1)
  }
  
  if(exists("tmp2")){
    tmp_all <- dplyr::bind_rows(tmp_all, tmp2)
  }
  
  if(exists("tmp3")){
    tmp_all <- dplyr::bind_rows(tmp_all, tmp3)
  }
  
  return(tmp_all)
}
