#' Calculate quantiles combined with FFT magnitude and angle coefficients
#'
#' Computes 101 quantiles (0 to 1 in steps of 0.01, by default) together with the magnitude and angle of FFT coefficients 0-99. The FFT features are those of \code{\link{fft_features}}: \code{fft_abs_*} is the squared magnitude unless \code{squared = FALSE}, angles are in degrees, and coefficients beyond the series' \code{floor(n/2) + 1} real-FFT coefficients are \code{NaN}.
#'
#' @importFrom stats quantile fft
#'
#' @param y \code{numeric} vector of values
#' @param quantiles \code{numeric} vector of quantiles to calculate. Defaults to \code{seq(0, 1, by = 0.01)}
#' @param squared \code{Boolean} specifying whether \code{fft_abs_*} is the squared magnitude (\code{|X[k]|^2}) rather than the magnitude (\code{|X[k]|}). Defaults to \code{TRUE}
#' @return \code{data.frame} of results
#' @author Trent Henderson
#' @references
#' Christ, M., Braun, N., Neuffer, J., and Kempa-Liehr, A.W. (2018).
#' Time Series FeatuRe Extraction on basis of Scalable Hypothesis tests
#' (tsfresh -- A Python package). \emph{Neurocomputing}, \strong{307}, 72--77.
#' \doi{10.1016/j.neucom.2018.03.067}
#' @export
#'

fftquantiles <- function(y, quantiles = seq(0, 1, by = 0.01), squared = TRUE){

  quantiles_df <- data.frame(
    names = paste0("quantile_", quantiles * 100),
    values = sapply(quantiles, function(q) as.numeric(stats::quantile(y, q)))
  )

  out <- rbind(quantiles_df, fft_coefficients(y, squared = squared))
  out$feature_set <- "fftquantiles"
  return(out)
}
