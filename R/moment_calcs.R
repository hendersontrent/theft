#' Calculate the skewness of a vector
#'
#' Moment-ratio skewness \eqn{g_1 = m_3 / m_2^{3/2}}, where \eqn{m_k} is the \eqn{k}-th sample central moment (with divisor \eqn{n}).
#'
#' @param y \code{numeric} vector of values
#' @return \code{numeric} scalar of skewness
#' @author Trent Henderson
#'
skewness <- function(y){
  d <- y - mean(y)
  skew <- mean(d^3) / mean(d^2)^(3 / 2)
  return(skew)
}

#' Calculate the kurtosis of a vector
#'
#' Moment-ratio kurtosis \eqn{b_2 = m_4 / m_2^2}, where \eqn{m_k} is the \eqn{k}-th sample central moment (with divisor \eqn{n}). This is not excess kurtosis: a Gaussian sample gives values near 3.
#'
#' @param y \code{numeric} vector of values
#' @return \code{numeric} scalar of kurtosis
#' @author Trent Henderson
#'
kurtosis <- function(y){
  d <- y - mean(y)
  kurt <- mean(d^4) / mean(d^2)^2
  return(kurt)
}
