#' Calculate a basic set of the four moments of the distribution
#'
#' Returns the mean, the sample variance (divisor \eqn{n - 1}), the moment-ratio skewness \eqn{g_1 = m_3 / m_2^{3/2}} and the moment-ratio kurtosis \eqn{b_2 = m_4 / m_2^2} (not excess kurtosis, so a Gaussian sample gives values near 3), where \eqn{m_k} is the \eqn{k}-th sample central moment with divisor \eqn{n}.
#'
#' @importFrom stats var
#'
#' @param y \code{numeric} vector of values
#' @return \code{data.frame} of results
#' @author Trent Henderson
#' @export
#'

moments <- function(y){
  moments_df <- data.frame(
    names = c("mean", "variance", "skewness", "kurtosis"),
    values = c(mean(y), stats::var(y), skewness(y), kurtosis(y))
  )
  
  moments_df$feature_set <- "moments"
  return(moments_df)
}
