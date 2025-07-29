#' Calculate a basic set of quantiles for an input time-series vector
#'
#' @importFrom stats quantile
#'
#' @param y \code{numeric} vector of values
#' @param quantiles \code{numeric} vector of quantiles to calculate. Defaults to \code{seq(0.01, 1, by = 0.01)}
#' @return \code{data.frame} of results
#' @author Trent Henderson
#' @export
#'

quantiles <- function(y, quantiles = seq(0.01, 1, by = 0.01)){
  quantiles_df <- data.frame(
    names = paste0("quantile_", quantiles * 100),
    values = sapply(quantiles, function(q) as.numeric(stats::quantile(y, q)))
  )
  
  quantiles_df$feature_set <- "quantiles"
  return(quantiles_df)
}
