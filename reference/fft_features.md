# Calculate relative spectral power in fixed frequency bands for an input time-series vector

Splits the frequency range from 0 to 0.5 cycles per sample (the Nyquist
frequency) into 100 equal-width bands and returns the fraction of the
series' spectral power in each, as `fft_power_1` (0 to 0.005 cycles per
sample) to `fft_power_100` (0.495 to 0.5). The bands sum to 1.

## Usage

``` r
fft_features(y, squared, phase = FALSE)
```

## Arguments

- y:

  `numeric` vector of values

- squared:

  Deprecated and ignored: the features are relative power, so there is
  no magnitude/squared-magnitude choice

- phase:

  `Boolean` specifying whether to also return each band's phase as its
  cosine and sine. Only meaningful when the series share a time origin.
  Defaults to `FALSE`

## Value

`data.frame` of results

## Details

The power spectrum is the periodogram of the mean-removed series,
computed with the FFT. A series of length `n` has periodogram ordinates
at frequencies `k/n`, `k = 1, ..., floor(n/2)`; each ordinate's power is
spread evenly over its frequency cell of width `1/n` and the result is
integrated over each band. Because the bands are defined in normalised
frequency rather than by FFT coefficient index, each feature measures
the same frequency range for series of any length (`n >= 2`), and no
coefficients are missing for short series. Short series have a coarse
frequency resolution (`1/n`), so neighbouring bands then share power
from the same ordinate. As the features are relative power, they don't
depend on the series' mean or scale (see
[`moments`](https://hendersontrent.github.io/theft/reference/moments.md)
for those). A constant series has no spectral power, so all its features
are `NaN`.

With `phase = TRUE`, each band's phase is also returned, as
`fft_phase_cos_b` and `fft_phase_sin_b` (the cosine and sine of the
phase, which represent it without the jump at +/-180 degrees). The
band's phase is the argument of the sum of its complex FFT coefficients,
each weighted by the fraction of its frequency cell in the band, so
strong coefficients dominate. Phase is measured relative to the first
sample, so shifting a series in time changes it: these features are only
meaningful when the series share a time origin (e.g., event-locked
trials or aligned segments). Bands with negligible power have no
well-defined phase and give `NaN`.

## Author

Trent Henderson
