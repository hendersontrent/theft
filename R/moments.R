#' Calculate a basic set of the four moments of the distribution
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
