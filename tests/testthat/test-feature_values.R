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
