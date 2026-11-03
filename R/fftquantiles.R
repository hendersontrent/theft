#' Calculate quantiles combined with relative spectral power in fixed frequency bands
#'
#' Computes 101 quantiles (0 to 1 in steps of 0.01, by default) together with the 100 relative band powers of \code{\link{fft_features}}.
#'
#' @importFrom stats quantile
#'
#' @param y \code{numeric} vector of values
#' @param quantiles \code{numeric} vector of quantiles to calculate. Defaults to \code{seq(0, 1, by = 0.01)}
#' @param squared Deprecated and ignored: the spectral features are relative power, so there is no magnitude/squared-magnitude choice
#' @return \code{data.frame} of results
#' @author Trent Henderson
#' @export
#'

fftquantiles <- function(y, quantiles = seq(0, 1, by = 0.01), squared){

  if(!missing(squared)){
    warn_squared_deprecated()
  }

  quantiles_df <- data.frame(
    names = paste0("quantile_", quantiles * 100),
    values = sapply(quantiles, function(q) as.numeric(stats::quantile(y, q)))
  )

  out <- rbind(quantiles_df, fft_band_power(y))
  out$feature_set <- "fftquantiles"
  return(out)
}
