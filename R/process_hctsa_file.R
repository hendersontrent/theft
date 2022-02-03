#' Load in hctsa formatted MATLAB files of time series data into a tidy format ready for feature extraction
#' 
#' @importFrom R.matlab readMat
#' @importFrom data.table rbindlist
#' @importFrom magrittr %>%
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
  
  # Parse 3 separate variables into single dataframe
  
  tmp <- data.table::rbindlist(d$keywords, use.names = TRUE)
  tmpVec <- tmp$V1
  tmp1 <- data.table::rbindlist(d$labels, use.names = TRUE)
  tmp1Vec <- tmp1$V1
  tmp2 <- d$timeSeriesData
  
  indices <- seq(from = 1, to = length(tmp2), by = 1)
  storage <- list()
  
  for(i in indices){
    tmpList <- tmp2[[i]][[1]]
    tmpListDat <- as.data.frame(tmpList)
    
    tmpList2 <- tmpListDat %>%
      dplyr::rename(values = V1) %>%
      dplyr::mutate(timepoint = dplyr::row_number()) %>%
      dplyr::mutate(group = tmpVec[i],
                    id = tmp1Vec[i])
    
    storage[[i]] <- tmpList2
  }
  
  myData <- data.table::rbindlist(storage, use.names = TRUE)
  
  if(!is.numeric(myData$values)){
    stop("Non-numerics identified in values.")
  }
  
  return(myData)
}