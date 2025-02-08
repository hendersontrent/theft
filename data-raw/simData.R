#' Helper function to add information to data frame
#' @param df \code{data.frame} containing simulated time series
#' @param id_num \code{integer} denoting id of the time series
#' @param process \code{character} denoting the data generating process
#' @return \code{data.frame} with time series information with additional columns
#' @author Trent Henderson
#' 

add_info <- function(df, id_num, process){
  tmp <- df
  tmp$timepoint <- seq.int(nrow(tmp))
  tmp$id <- paste0(process, "_", id_num)
  tmp$process <- process
  return(tmp)
}


#' Function to simulate statistical processes needed to calculate time-series features on
#' @importFrom stats rnorm arima.sim
#' @import dplyr
#' @param ts_length \code{integer} denoting the time-series length to produce. Defaults to \code{1000}
#' @param num_ts \code{integer} denoting the number of time series to generate for each \code{process}. Defaults to \code{30}
#' @param process \code{character} or \code{character vector} denoting the statistical process to simulate. Defaults to \code{"Gaussian Noise"}
#' @return \code{data.frame} with the simulated time-series process values
#' @author Trent Henderson
#' 

simulation_engine <- function(ts_length = 1000, num_ts = 30,
                              process = c("Gaussian Noise", "Noisy Sinusoid", "Random Walk",
                                          "AR(1)", "MA(1)", "ARMA(1,1)")){
  
  process <- match.arg(process)
  
  #---------- Main calcs ---------------
  
  storage <- list()
  
  for(i in 1:num_ts){
    
    set.seed(i)
    
    if(process == "Gaussian Noise"){
      outData <- data.frame(values = stats::rnorm(ts_length, mean = 0, sd = 1))
      outData <- add_info(outData, i, process)
    }
    
    if(process == "Noisy Sinusoid"){
      n <- seq(from = 1, to = ts_length, by = 1)
      outData <- data.frame(values = sin(n) + stats::rnorm(ts_length, mean = 0, sd = 0.25))
      outData <- add_info(outData, i, process)
    }
    
    if(process == "Random Walk"){
      outData <- data.frame(values = cumsum(stats::rnorm(ts_length, mean = 0, sd = 1)))
      outData <- add_info(outData, i, process)
    }
    
    if(process == "AR(1)"){
      themod <- list(order = c(1, 0, 0), ar = 0.4)
      outData <- data.frame(values = c(stats::arima.sim(n = ts_length, model = themod, sd = 0.1)))
      outData <- add_info(outData, i, process)
    }
    
    if(process == "MA(1)"){
      themod <- list(order = c(0, 0, 1), ma = 0.4)
      outData <- data.frame(values = c(stats::arima.sim(n = ts_length, model = themod, sd = 0.1)))
      outData <- add_info(outData, i, process)
    }
    
    if(process == "ARMA(1,1)"){
      themod <- list(order = c(1, 0, 1), ar = 0.4, ma = 0.4)
      outData <- data.frame(values = c(5 + stats::arima.sim(n = ts_length, model = themod)))
      outData <- add_info(outData, i, process)
    }
    storage[[i]] <- outData
  }
   
  outData <- do.call("rbind", storage)
  return(outData)
}

# Run the function

storage <- list()

for(i in c("Gaussian Noise", "Noisy Sinusoid", "Random Walk","AR(1)", "MA(1)", "ARMA(1,1)")){
  tmp <- simulation_engine(100, 30, i)
  storage[[i]] <- tmp
}

simData <- do.call("rbind", storage)
simData <- tsibble::as_tsibble(simData, key = c("id", "process"), index = "timepoint")

# Save output to package

write.csv(simData, "data-raw/simData.csv")
usethis::use_data(simData, overwrite = TRUE)
