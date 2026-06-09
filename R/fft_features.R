#' Calculate FFT absolute value and angle coefficients for an input time-series vector
#'
#' Computes the absolute value and angle of FFT coefficients 0-99, following the same convention as \code{tsfresh}'s \code{fft_coefficient} feature with \code{attr = "abs"} and \code{attr = "angle"}.
#'
#' @importFrom stats fft
#'
#' @param y \code{numeric} vector of values
#' @param squared \code{Boolean} specifying whether to compute squared magnitude (\code{|X[k]|^2}) instead of absolute value (\code{|X[k]|}). Defaults to \code{TRUE}
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

  n_coeff <- 100L
  fft_result <- stats::fft(y)
  n_rfft <- floor(length(y) / 2L) + 1L

  fft_vals <- complex(real = 0, imaginary = 0, length.out = n_coeff)
  fft_vals[seq_len(min(n_coeff, n_rfft))] <- fft_result[seq_len(min(n_coeff, n_rfft))]

  coeffs <- 0:(n_coeff - 1L)

  abs_vals <- if(squared) Mod(fft_vals)^2 else Mod(fft_vals)

  return(
    rbind(
      data.frame(names = paste0("fft_abs_", coeffs), values = abs_vals, feature_set = "fft"),
      data.frame(names = paste0("fft_angle_", coeffs), values = Arg(fft_vals) * 180 / pi, feature_set = "fft")
    )
  )
}
