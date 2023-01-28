#' This function rescales a vector of numerical values into the unit interval [0,1]
#' \deqn{z_i = \frac{x_i - \min(\mathbf{x})}{\max(\mathbf{x}) - \min(\mathbf{x})}}
#' @importFrom scales rescale
#' @param x a numeric vector, preferably of feature values computed by other \code{theft} package functions
#' @return \code{numeric} vector
#' @author Trent Henderson
#'

minmax_scaler <- function(x){
  
  x1 <- as.vector(x) # Catches class "ts" cases
  x_new <- scales::rescale(x1, to = c(0, 1))
  return(x_new)
}

#' This function rescales a vector of numerical values into z-scores and then into the unit interval [0,1]
#' \deqn{z_i = \frac{x_i - \mu}{\sigma}}
#' @importFrom stats sd
#' @param x a numeric vector, preferably of feature values computed by other \code{theft} package functions
#' @param unitInt Booelan whether to rescale outputs into unit interval \code{[0,1]}. Defaults to \code{TRUE}
#' @return \code{numeric} vector
#' @author Trent Henderson
#'

zscore_scaler <- function(x, unitInt = TRUE){
  
  x1 <- as.vector(x) # Catches class "ts" cases
  x_new <- (x1 - mean(x1, na.rm = TRUE)) / stats::sd(x1, na.rm = TRUE)
  
  if(unitInt){
    x_new <- scales::rescale(x_new, to = c(0, 1))
  }
  
  return(x_new)
}

#' This function rescales a vector of numerical values using a Sigmoidal transformation
#' \deqn{z_i = \left[1 + \exp(-\frac{x_i - \mu}{\sigma})\right]^{-1}}
#' @importFrom scales rescale
#' @importFrom stats sd
#' @param x a numeric vector, preferably of feature values computed by other \code{theft} package functions
#' @param unitInt Booelan whether to rescale Sigmoidal outputs into unit interval \code{[0,1]}. Defaults to \code{TRUE}
#' @return \code{numeric} vector
#' @author Trent Henderson
#'

sigmoid_scaler <- function(x, unitInt = TRUE){
  
  x1 <- as.vector(x) # Catches class "ts" cases
  x_new <- 1 / (1 + exp(-((x1 - mean(x1, na.rm = TRUE)) / stats::sd(x1, na.rm = TRUE))))
  
  if(unitInt){
    x_new <- scales::rescale(x_new, to = c(0, 1))
  }
  
  return(x_new)
}

#' This function rescales a vector of numerical values with an outlier-robust Sigmoidal transformation and then into the unit interval [0,1]
#' \deqn{z_i = \left[1 + \exp\left(-\frac{x_i - \mathrm{median}(\mathbf{x})}{\mathrm{IQR}(\mathbf{x})/{1.35}}\right)\right]^{-1}}
#' @importFrom scales rescale
#' @importFrom stats median
#' @importFrom stats IQR
#' @param x a numeric vector, preferably of feature values computed by other \code{theft} package functions
#' @param unitInt Booelan whether to rescale Sigmoidal outputs into unit interval \code{[0,1]}. Defaults to \code{TRUE}
#' @return \code{numeric} vector
#' @author Trent Henderson
#'

robustsigmoid_scaler <- function(x, unitInt = TRUE){
  
  x1 <- as.vector(x) # Catches class "ts" cases
  x_new <- 1 / (1 + exp(-((x1 - stats::median(x1, na.rm = TRUE)) / (stats::IQR(x1, na.rm = TRUE) / 1.35))))
  
  if(unitInt){
    x_new <- scales::rescale(x_new, to = c(0, 1))
  }
  
  return(x_new)
}
