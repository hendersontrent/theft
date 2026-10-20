context("test-feature_values")

test_that("time series containing NAs are removed before feature calculation", {

  set.seed(1)
  d <- rbind(data.frame(id = "a", timepoint = 1:50, values = stats::rnorm(50)),
             data.frame(id = "b", timepoint = 1:50, values = c(stats::rnorm(10), NA, stats::rnorm(39))))

  d <- tsibble::as_tsibble(d, key = "id", index = "timepoint")

  expect_message(f <- calculate_features(d, feature_set = "catch22", warn = FALSE),
                 "Removed 1 time series due to non-real values.")

  expect_equal(unique(f$id), "a")
  expect_equal(nrow(f), 22)
})

test_that("seed makes stochastic features reproducible", {

  skip_on_cran()

  set.seed(10)
  d <- rbind(data.frame(id = "a", timepoint = 1:100, values = cumsum(stats::rnorm(100))),
             data.frame(id = "b", timepoint = 1:100, values = stats::rnorm(100)))

  d <- tsibble::as_tsibble(d, key = "id", index = "timepoint")

  set.seed(1)
  f1 <- calculate_features(d, feature_set = "tsfeatures", seed = 123, warn = FALSE)
  set.seed(2)
  f2 <- calculate_features(d, feature_set = "tsfeatures", seed = 123, warn = FALSE)

  expect_equal(f1$values[f1$names == "spreadrandomlocal_meantaul"],
               f2$values[f2$names == "spreadrandomlocal_meantaul"])
})
