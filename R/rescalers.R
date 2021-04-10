#' This function rescales a vector of numerical values into the unit interval [0,1]
#' 
#' @importFrom scales rescale
#' @param x a numeric vector, preferably of feature values computed by other {theft} package functions
#' @return x a numeric vector, rescaled into the [0,1] unit interval
#' @author Trent Henderson
#' @export
#' @examples
#' x <- 1 + 0.5 * 1:1000 + arima.sim(list(ma = 0.5), n = 1000)
#' outs <- minmax_scaler(x)
#'

minmax_scaler <- function(x){
  
  x1 <- as.vector(x) # Catches class "ts" cases
  
  x_new <- scales::rescale(x1, to = c(0,1))
  return(x_new)
}

#' This function rescales a vector of numerical values into z-scores
#'
#' @param x a numeric vector, preferably of feature values computed by other {theft} package functions
#' @return x a numeric vector, rescaled into z-scores
#' @author Trent Henderson
#' @export
#' @examples
#' x <- 1 + 0.5 * 1:1000 + arima.sim(list(ma = 0.5), n = 1000)
#' outs <- zscore_scaler(x)
#'

zscore_scaler <- function(x){
  
  x1 <- as.vector(x) # Catches class "ts" cases
  
  x_new <- (x-mean(x1, na.rm = TRUE))/sd(x1, na.rm = TRUE)
  return(x_new)
}

#' This function rescales a vector of numerical values with a Sigmoidal transformation
#' 
#' @importFrom scales rescale
#' @param x a numeric vector, preferably of feature values computed by other {theft} package functions
#' @param unitInt Booelan whether to rescale Sigmoidal outputs into unit interval [0,1]. Defaults to TRUE
#' @return x a numeric rescaled vector
#' @author Trent Henderson
#' @export
#' @examples
#' x <- 1 + 0.5 * 1:1000 + arima.sim(list(ma = 0.5), n = 1000)
#' outs <- sigmoid_scaler(x)
#'

sigmoid_scaler <- function(x, unitInt = TRUE){
  
  x1 <- as.vector(x) # Catches class "ts" cases
  
  x_new <- 1/(1+exp(-((x1-mean(x1, na.rm = TRUE))/sd(x1, na.rm = TRUE))))
  
  if(unitInt){
    x_new <- scales::rescale(x_new, to = c(0,1))
  } else{
    x_new
  }
  
  return(x_new)
}

#' This function rescales a vector of numerical values with an outlier-robust Sigmoidal transformation
#' 
#' @importFrom scales rescale
#' @param x a numeric vector, preferably of feature values computed by other {theft} package functions
#' @param unitInt Booelan whether to rescale Sigmoidal outputs into unit interval [0,1]. Defaults to TRUE
#' @return x a numeric rescaled vector
#' @author Trent Henderson
#' @export
#' @examples
#' x <- 1 + 0.5 * 1:1000 + arima.sim(list(ma = 0.5), n = 1000)
#' outs <- robustsigmoid_scaler(x)
#'

robustsigmoid_scaler <- function(x, unitInt = TRUE){
  
  x1 <- as.vector(x) # Catches class "ts" cases
  
  x_new <- 1/(1+exp(-((x1-median(x1, na.rm = TRUE))/(IQR(x1, na.rm = TRUE)/1.35))))
  
  if(unitInt){
    x_new <- scales::rescale(x_new, to = c(0,1))
  } else{
    x_new
  }
  
  return(x_new)
}

#' This function rescales a vector of numerical values by subtracting the mean
#'
#' @param x a numeric vector, preferably of feature values computed by other {theft} package functions
#' @author Trent Henderson
#' @export
#' @examples
#' x <- 1 + 0.5 * 1:1000 + arima.sim(list(ma = 0.5), n = 1000)
#' outs <- mean_scaler(x)
#'

mean_scaler <- function(x){
  
  x1 <- as.vector(x) # Catches class "ts" cases
  
  x_new <- x1-mean(x1, na.rm = TRUE)
  return(x_new)
}
