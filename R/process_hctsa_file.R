#---------------- Helper function ------------------

hctsa_iterator <- function(labels, keywords, timeSeriesData, x){
  
  tmp <- timeSeriesData[[x]][[1]]
  tmp <- as.data.frame(tmp)
  
  tmp <- tmp %>%
    dplyr::rename(values = V1) %>%
    dplyr::mutate(timepoint = dplyr::row_number()) %>%
    dplyr::mutate(group = keywords[x],
                  id = labels[x])
  
  return(tmp)
}

#---------------- Core function ------------------

#' Load in hctsa formatted MATLAB files of time series data into a tidy format ready for feature extraction
#' 
#' @importFrom R.matlab readMat
#' @importFrom purrr map_df
#' @import dplyr
#' @param data a string specifying the filepath to the \code{MATLAB} file to parse
#' @return an object of class dataframe in tidy format
#' @author Trent Henderson
#' @export
#' @examples
#' \dontrun{
#' myfile <- process_hctsa_file(
#'   "https://cloudstor.aarnet.edu.au/plus/s/6sRD6IPMJyZLNlN/download"
#'   )
#' }
#' 

process_hctsa_file <- function(data){
  
  if(!is.character(data)){
    stop("data should be a string specifying the filepath to the MATLAB file to parse")
  }
  
  # Read MATLAB file into list
  
  d <- R.matlab::readMat(data)
  theNames <- names(d)
  correctNames <- c("timeSeriesData", "labels", "keywords")
  
  if(length(theNames) != 3){
    stop("3 variables should be 'timeSeriesData', 'labels', and 'keywords'.")
  }
  
  if(setequal(theNames, correctNames) == FALSE){
    stop("3 variables should be 'timeSeriesData', 'labels', and 'keywords'.")
  }
  
  # Parse into single tidy dataframe
  
  keywords <- as.vector(do.call(rbind, d$keywords))
  labels <- as.vector(do.call(rbind, d$labels))
  timeSeriesData <- d$timeSeriesData
  
  myData <- 1:length(timeSeriesData) %>%
    purrr::map_df(~ hctsa_iterator(labels, keywords, timeSeriesData, .x))
  
  if(!is.numeric(myData$values)){
    stop("Non-numerics identified in values.")
  }
  
  return(myData)
}
