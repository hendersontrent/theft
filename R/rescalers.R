#' Rescales a numeric vector into the unit interval [0,1]
#' 
#' \eqn{z_{i} = \frac{x_{i} - \text{min}(\mathbf{x})}{\text{max}(\mathbf{x}) - \text{min}(\mathbf{x})}}
#' 
#' @importFrom scales rescale
#' @param x \code{numeric vector}
#' @return \code{numeric vector}
#' @author Trent Henderson
#'

minmax_scaler <- function(x){
  x1 <- as.vector(x) # Catches class "ts" cases
  x_new <- scales::rescale(x1, to = c(0, 1))
  return(x_new)
}

#' Rescales a numeric vector into z-scores and then into the unit interval [0,1]
#' 
#' \eqn{z_{i} = \frac{x_{i} - \mu}{\sigma}}
#' 
#' @importFrom stats sd
#' @importFrom scales rescale
#' @param x \code{numeric vector}
#' @return \code{numeric} vector
#' @author Trent Henderson
#'

zscore_scaler <- function(x){
  x1 <- as.vector(x) # Catches class "ts" cases
  x_new <- (x1 - mean(x1, na.rm = TRUE)) / stats::sd(x1, na.rm = TRUE)
  return(x_new)
}

#' Rescales a numeric vector using a Sigmoidal transformation
#' 
#' \eqn{z_{i} = \left[1 + \exp(-\frac{x_{i} - \mu}{\sigma})\right]^{-1}}
#' 
#' @importFrom stats sd
#' @importFrom scales rescale
#' @param x \code{numeric vector}
#' @return \code{numeric vector}
#' @author Trent Henderson
#'

sigmoid_scaler <- function(x){
  x1 <- as.vector(x) # Catches class "ts" cases
  x_new <- 1 / (1 + exp(-((x1 - mean(x1, na.rm = TRUE)) / stats::sd(x1, na.rm = TRUE))))
  return(x_new)
}

#' Rescales a numeric vector using an outlier-robust Sigmoidal transformation and then into the unit interval [0,1]
#' 
#' \eqn{z_{i} = \left[1 + \exp\left(-\frac{x_{i} - \text{median}(\mathbf{x})}{\text{IQR}(\mathbf{x})/{1.35}}\right)\right]^{-1}}
#' 
#' @importFrom stats median IQR
#' @importFrom scales rescale
#' @param x \code{numeric vector}
#' @return \code{numeric vector}
#' @references Fulcher, Ben D., Little, Max A., and Jones, Nick S. Highly Comparative Time-Series Analysis: The Empirical Structure of Time Series and Their Methods. Journal of The Royal Society Interface 10(83), (2013).
#' @author Trent Henderson
#'

robustsigmoid_scaler <- function(x){
  x1 <- as.vector(x) # Catches class "ts" cases
  x_new <- 1 / (1 + exp(-((x1 - stats::median(x1, na.rm = TRUE)) / (stats::IQR(x1, na.rm = TRUE) / 1.35))))
  return(x_new)
}
