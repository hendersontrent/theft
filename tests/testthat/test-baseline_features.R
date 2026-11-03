context("test-baseline_features")

test_that("skewness and kurtosis are the moment-ratio estimators g1 and b2", {

  # y = (1, 2, 3, 10): deviations (-3, -2, -1, 6), m2 = 12.5, m3 = 45, m4 = 348.5

  y <- c(1, 2, 3, 10)
  m <- moments(y)
  expect_equal(m$values[m$names == "skewness"], 45 / 12.5^1.5)
  expect_equal(m$values[m$names == "kurtosis"], 348.5 / 12.5^2)
  expect_equal(m$values[m$names == "variance"], stats::var(y))

  # Symmetric two-point distribution: skewness 0, kurtosis 1

  m2 <- moments(rep(c(-1, 1), 10))
  expect_equal(m2$values[m2$names == "skewness"], 0)
  expect_equal(m2$values[m2$names == "kurtosis"], 1)
})

test_that("FFT coefficients match the real FFT and are NaN beyond it", {

  set.seed(1)
  y <- stats::rnorm(20) # floor(20 / 2) + 1 = 11 real-FFT coefficients
  X <- stats::fft(y)[1:11]

  f_abs <- fft_features(y, squared = FALSE)
  f_sq <- fft_features(y)

  get <- function(f, prefix){f$values[f$names %in% paste0(prefix, 0:99)][order(as.integer(sub(prefix, "", f$names[f$names %in% paste0(prefix, 0:99)])))]}
  abs_vals <- get(f_abs, "fft_abs_")
  sq_vals <- get(f_sq, "fft_abs_")
  angle_vals <- get(f_abs, "fft_angle_")

  expect_equal(nrow(f_abs), 200)
  expect_equal(unique(f_abs$feature_set), "fft")
  expect_equal(abs_vals[1], abs(sum(y)))
  expect_equal(abs_vals[1:11], Mod(X))
  expect_equal(sq_vals[1:11], Mod(X)^2)
  expect_equal(angle_vals[1:11], Arg(X) * 180 / pi)
  expect_true(all(is.nan(abs_vals[12:100])))
  expect_true(all(is.nan(sq_vals[12:100])))
  expect_true(all(is.nan(angle_vals[12:100])))

  # Long series have all 100 coefficients

  expect_false(anyNA(fft_features(stats::rnorm(300))$values))
})

test_that("fftquantiles has 101 quantiles and the same FFT values as fft_features", {

  set.seed(2)
  y <- stats::rnorm(50)
  fq <- fftquantiles(y)
  ff <- fft_features(y)

  expect_equal(sum(grepl("^quantile_", fq$names)), 101)
  expect_equal(unique(fq$feature_set), "fftquantiles")
  expect_equal(fq$values[grepl("^fft_", fq$names)], ff$values)
  expect_equal(fq$names[grepl("^fft_", fq$names)], ff$names)
})
