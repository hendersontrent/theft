#' Calculate FFT magnitude and angle coefficients for an input time-series vector
#'
#' Computes the magnitude and angle of FFT coefficients 0-99. The coefficients are those of \code{tsfresh}'s \code{fft_coefficient} feature (the real FFT of the series), with two differences in the defaults: \code{fft_abs_*} is the squared magnitude \code{|X[k]|^2} unless \code{squared = FALSE}, which gives \code{tsfresh}'s \code{attr = "abs"}; \code{fft_angle_*} is in degrees, as \code{tsfresh}'s \code{attr = "angle"}. A series of length \code{n} has only \code{floor(n/2) + 1} real-FFT coefficients, so coefficients beyond that are \code{NaN}, as in \code{tsfresh}.
#'
#' @importFrom stats fft
#'
#' @param y \code{numeric} vector of values
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

fft_features <- function(y, squared = TRUE){
  coefs <- fft_coefficients(y, squared = squared)
  coefs$feature_set <- "fft"
  return(coefs)
}

#' Magnitude and angle of the first 100 real-FFT coefficients
#'
#' @importFrom stats fft
#'
#' @param y \code{numeric} vector of values
#' @param squared \code{Boolean} specifying whether to return the squared magnitude
#' @param n_coeff \code{integer} number of coefficients. Defaults to \code{100}
#' @return \code{data.frame} with columns \code{names} and \code{values}
#' @author Trent Henderson
#' @noRd
#'

fft_coefficients <- function(y, squared = TRUE, n_coeff = 100L){

  fft_result <- stats::fft(y)
  n_avail <- min(n_coeff, floor(length(y) / 2L) + 1L)
  coeffs <- 0:(n_coeff - 1L)

  # Coefficients the series doesn't have are NaN (as in tsfresh), not 0

  abs_vals <- rep(NaN, n_coeff)
  angle_vals <- rep(NaN, n_coeff)
  kept <- fft_result[seq_len(n_avail)]
  abs_vals[seq_len(n_avail)] <- if(squared) Mod(kept)^2 else Mod(kept)
  angle_vals[seq_len(n_avail)] <- Arg(kept) * 180 / pi # Degrees, as in tsfresh

  return(
    rbind(
      data.frame(names = paste0("fft_abs_", coeffs), values = abs_vals),
      data.frame(names = paste0("fft_angle_", coeffs), values = angle_vals)
    )
  )
}
