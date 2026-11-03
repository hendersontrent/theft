#' Calculate relative spectral power in fixed frequency bands for an input time-series vector
#'
#' Splits the frequency range from 0 to 0.5 cycles per sample (the Nyquist frequency) into 100 equal-width bands and returns the fraction of the series' spectral power in each, as \code{fft_power_1} (0 to 0.005 cycles per sample) to \code{fft_power_100} (0.495 to 0.5). The bands sum to 1.
#'
#' The power spectrum is the periodogram of the mean-removed series, computed with the FFT. A series of length \code{n} has periodogram ordinates at frequencies \code{k/n}, \code{k = 1, ..., floor(n/2)}; each ordinate's power is spread evenly over its frequency cell of width \code{1/n} and the result is integrated over each band. Because the bands are defined in normalised frequency rather than by FFT coefficient index, each feature measures the same frequency range for series of any length (\code{n >= 2}), and no coefficients are missing for short series. Short series have a coarse frequency resolution (\code{1/n}), so neighbouring bands then share power from the same ordinate. As the features are relative power, they don't depend on the series' mean or scale (see \code{\link{moments}} for those). A constant series has no spectral power, so all its features are \code{NaN}.
#'
#' @importFrom stats fft
#'
#' @param y \code{numeric} vector of values
#' @param squared Deprecated and ignored: the features are relative power, so there is no magnitude/squared-magnitude choice
#' @return \code{data.frame} of results
#' @author Trent Henderson
#' @export
#'

fft_features <- function(y, squared){
  if(!missing(squared)){
    warn_squared_deprecated()
  }
  out <- fft_band_power(y)
  out$feature_set <- "fft"
  return(out)
}

#' Relative spectral power in equal-width frequency bands
#'
#' @importFrom stats fft approx
#'
#' @param y \code{numeric} vector of values
#' @param n_bands \code{integer} number of equal-width bands between 0 and 0.5 cycles per sample. Defaults to \code{100}
#' @return \code{data.frame} with columns \code{names} and \code{values}
#' @author Trent Henderson
#' @noRd
#'

fft_band_power <- function(y, n_bands = 100L){

  n <- length(y)
  band_names <- paste0("fft_power_", seq_len(n_bands))

  if(n < 2){
    return(data.frame(names = band_names, values = rep(NaN, n_bands)))
  }

  # One-sided periodogram of the mean-removed series at k/n, k = 1, ..., floor(n/2)

  X <- stats::fft(y - mean(y))
  k <- seq_len(floor(n / 2))
  pow <- Mod(X[k + 1])^2

  # Spread each ordinate's power evenly over its cell [(k - 1/2)/n, (k + 1/2)/n], clipped at 0.5.
  # The Nyquist ordinate (n even) has half a cell and isn't doubled, so all cells share one density convention

  upper <- pmin((k + 0.5) / n, 0.5)
  pow <- ifelse(2 * k == n, pow, 2 * pow)
  total <- sum(pow)

  # Integrate the piecewise-constant density over each band via its cumulative distribution

  edges <- seq(0, 0.5, length.out = n_bands + 1)
  cum_at_edges <- stats::approx(x = c(0, 0.5 / n, upper), y = c(0, 0, cumsum(pow)), xout = edges, ties = "ordered")$y
  band_power <- diff(cum_at_edges) / total

  if(!is.finite(total) || total <= 0){
    band_power <- rep(NaN, n_bands)
  }

  return(data.frame(names = band_names, values = band_power))
}

#' Warn that the squared argument no longer has an effect
#'
#' @noRd
#'

warn_squared_deprecated <- function(){
  warning("`squared` is deprecated and ignored: the \"fft\" features are now relative spectral power in fixed frequency bands.", call. = FALSE)
}
