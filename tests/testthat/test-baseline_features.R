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

band <- function(f, b){f$values[f$names == paste0("fft_power_", b)]}

test_that("fft band powers sum to 1 for any length, with no missing values", {

  set.seed(1)
  for(n in c(2, 3, 10, 51, 199, 200, 1000)){
    f <- fft_features(stats::rnorm(n))
    expect_equal(nrow(f), 100)
    expect_equal(f$names, paste0("fft_power_", 1:100))
    expect_equal(unique(f$feature_set), "fft")
    expect_false(anyNA(f$values))
    expect_true(all(f$values >= 0))
    expect_equal(sum(f$values), 1)
  }
})

test_that("a sinusoid's power is in the same frequency band whatever the series length", {

  # f = 0.1 cycles per sample sits on the edge between band 20 (0.095-0.1) and band 21 (0.1-0.105)

  for(n in c(100, 1000)){
    f <- fft_features(sin(2 * pi * 0.1 * seq_len(n)))
    expect_equal(band(f, 20), 0.5)
    expect_equal(band(f, 21), 0.5)
    expect_equal(sum(f$values[-(20:21)]), 0, tolerance = 1e-10)
  }

  # With n = 20 the frequency resolution is 0.05, so the same power spreads evenly over bands 16-25

  f <- fft_features(sin(2 * pi * 0.1 * seq_len(20)))
  expect_equal(f$values[16:25], rep(0.1, 10))
  expect_equal(sum(f$values[-(16:25)]), 0, tolerance = 1e-10)
})

test_that("fft band powers don't depend on the mean or scale, and a constant series gives NaN", {

  set.seed(2)
  y <- stats::rnorm(80)
  expect_equal(fft_features(3 * y + 5)$values, fft_features(y)$values)
  expect_true(all(is.nan(fft_features(rep(2, 30))$values)))
})

test_that("squared is deprecated", {

  y <- stats::rnorm(30)
  expect_warning(fft_features(y, squared = TRUE), "deprecated")
  expect_warning(fftquantiles(y, squared = FALSE), "deprecated")
  expect_warning(calculate_features(tsibble::as_tsibble(data.frame(id = "a", t = 1:30, values = y), key = "id", index = "t"),
                                    feature_set = "fft", squared = TRUE), "deprecated")
})

test_that("fftquantiles has 101 quantiles and the same band powers as fft_features", {

  set.seed(2)
  y <- stats::rnorm(50)
  fq <- fftquantiles(y)
  ff <- fft_features(y)

  expect_equal(nrow(fq), 201)
  expect_equal(sum(grepl("^quantile_", fq$names)), 101)
  expect_equal(unique(fq$feature_set), "fftquantiles")
  expect_equal(fq$values[grepl("^fft_", fq$names)], ff$values)
  expect_equal(fq$names[grepl("^fft_", fq$names)], ff$names)
})
