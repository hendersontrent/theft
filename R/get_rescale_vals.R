#' Calculate central tendency and spread values for all numeric columns in a dataset
#' 
#' @param data \code{data.frame} containing data to normalise
#' @return \code{list} of central tendency and spread values
#' @author Trent Henderson
#' 

get_rescale_vals <- function(data){
  
  ct <- data %>%
    dplyr::summarise_if(is.numeric, mean)
  
  ct <- as.numeric(ct[1, ])
  
  spreads <- data %>%
    dplyr::summarise_if(is.numeric, sd)
  
  spreads <- as.numeric(spreads[1, ])
  
  outs <- list(ct, spreads)
  names(outs) <- c("CentralTendency", "Spread")
  return(outs)
}

#' Calculate z-score for all columns in a dataset using train set central tendency and spread
#' 
#' @param data \code{data.frame} containing data to normalise
#' @param rescalers \code{list} containing central tendency and spread values for the train set
#' @return \code{data.frame} of rescaled data
#' @author Trent Henderson
#' 

rescale_zscore <- function(data, rescalers){
  data_scaled <- data
  
  for(i in 2:ncol(data_scaled)){
    data_scaled[, i] <- (data_scaled[, i] - rescalers$CentralTendency[[i - 1]]) / rescalers$Spread[[i - 1]]
  }
  
  return(data_scaled)
}
