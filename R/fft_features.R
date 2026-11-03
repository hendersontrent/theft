#' Calculate relative spectral power in fixed frequency bands for an input time-series vector
#'
#' Splits the frequency range from 0 to 0.5 cycles per sample (the Nyquist frequency) into 100 equal-width bands and returns the fraction of the series' spectral power in each, as \code{fft_power_1} (0 to 0.005 cycles per sample) to \code{fft_power_100} (0.495 to 0.5). The bands sum to 1.
#'
#' The power spectrum is the periodogram of the mean-removed series, computed with the FFT. A series of length \code{n} has periodogram ordinates at frequencies \code{k/n}, \code{k = 1, ..., floor(n/2)}; each ordinate's power is spread evenly over its frequency cell of width \code{1/n} and the result is integrated over each band. Because the bands are defined in normalised frequency rather than by FFT coefficient index, each feature measures the same frequency range for series of any length (\code{n >= 2}), and no coefficients are missing for short series. Short series have a coarse frequency resolution (\code{1/n}), so neighbouring bands then share power from the same ordinate. As the features are relative power, they don't depend on the series' mean or scale (see \code{\link{moments}} for those). A constant series has no spectral power, so all its features are \code{NaN}.
#'
#' With \code{phase = TRUE}, each band's phase is also returned, as \code{fft_phase_cos_b} and \code{fft_phase_sin_b} (the cosine and sine of the phase, which represent it without the jump at +/-180 degrees). The band's phase is the argument of the sum of its complex FFT coefficients, each weighted by the fraction of its frequency cell in the band, so strong coefficients dominate. Phase is measured relative to the first sample, so shifting a series in time changes it: these features are only meaningful when the series share a time origin (e.g., event-locked trials or aligned segments). Bands with negligible power have no well-defined phase and give \code{NaN}.
#'
#' @importFrom stats fft
#'
#' @param y \code{numeric} vector of values
#' @param squared Deprecated and ignored: the features are relative power, so there is no magnitude/squared-magnitude choice
#' @param phase \code{Boolean} specifying whether to also return each band's phase as its cosine and sine. Only meaningful when the series share a time origin. Defaults to \code{FALSE}
#' @return \code{data.frame} of results
#' @author Trent Henderson
#' @export
#'

fft_features <- function(y, squared, phase = FALSE){
  if(!missing(squared)){
    warn_squared_deprecated()
  }
  out <- fft_bands(y, phase = phase)
  out$feature_set <- "fft"
  return(out)
}

#' Relative spectral power, and optionally phase, in equal-width frequency bands
#'
#' @importFrom stats fft
#'
#' @param y \code{numeric} vector of values
#' @param phase \code{Boolean} specifying whether to also return each band's phase as its cosine and sine
#' @param n_bands \code{integer} number of equal-width bands between 0 and 0.5 cycles per sample. Defaults to \code{100}
#' @return \code{data.frame} with columns \code{names} and \code{values}
#' @author Trent Henderson
#' @noRd
#'

fft_bands <- function(y, phase = FALSE, n_bands = 100L){

  n <- length(y)
  b <- seq_len(n_bands)
  band_power <- band_cos <- band_sin <- rep(NaN, n_bands)

  if(n >= 2){

    # FFT of the mean-removed series at frequencies k/n, k = 1, ..., floor(n/2)

    k <- seq_len(floor(n / 2))
    X <- stats::fft(y - mean(y))[k + 1]

    # One-sided power; the Nyquist ordinate (n even) isn't doubled and has half a cell

    pow <- ifelse(2 * k == n, 1, 2) * Mod(X)^2
    total <- sum(pow)

    # Each ordinate is spread evenly over its cell [(k - 1/2)/n, (k + 1/2)/n], clipped at 0.5;
    # a band takes the fraction of each cell that it overlaps

    lo <- (k - 0.5) / n
    hi <- pmin((k + 0.5) / n, 0.5)
    edges <- seq(0, 0.5, length.out = n_bands + 1)

    if(is.finite(total) && total > 0){
      tol <- sqrt(.Machine$double.eps) * sum(Mod(X))
      for(i in b){
        w <- pmax(0, pmin(hi, edges[i + 1]) - pmax(lo, edges[i])) / (hi - lo)
        band_power[i] <- sum(w * pow) / total
        if(phase){
          z <- sum(w * X)
          if(Mod(z) > tol){
            band_cos[i] <- Re(z) / Mod(z)
            band_sin[i] <- Im(z) / Mod(z)
          }
        }
      }
    }
  }

  out <- data.frame(names = paste0("fft_power_", b), values = band_power)

  if(phase){
    out <- rbind(out,
                 data.frame(names = paste0("fft_phase_cos_", b), values = band_cos),
                 data.frame(names = paste0("fft_phase_sin_", b), values = band_sin))
  }

  return(out)
}

#' Warn that the squared argument no longer has an effect
#'
#' @noRd
#'

warn_squared_deprecated <- function(){
  warning("`squared` is deprecated and ignored: the \"fft\" features are now relative spectral power in fixed frequency bands.", call. = FALSE)
}
