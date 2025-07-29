#' Calculate a skewness of a vector
#'
#' @importFrom stats sd
#'
#' @param y \code{numeric} vector of values
#' @return \code{numeric} scalar of skewness
#' @author Trent Henderson
#'
skewness <- function(y){
  skew <- sum((y - mean(y))^3) / ((length(y) - 1) * stats::sd(y)^3)
  return(skew)
}

#' Calculate a kurtosis of a vector
#'
#' @importFrom stats sd
#'
#' @param y \code{numeric} vector of values
#' @return \code{numeric} scalar of kurtosis
#' @author Trent Henderson
#'
kurtosis <- function(y){
  kurt <- sum((y - mean(y))^4) / ((length(y) - 1) * stats::sd(y)^4)
  return(kurt)
}
