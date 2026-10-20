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

test_that("tsfeatures treats series sharing an id across groups as separate series", {

  skip_on_cran()

  set.seed(10)
  d <- rbind(data.frame(id = "s1", group = "A", timepoint = 1:100, values = cumsum(stats::rnorm(100))),
             data.frame(id = "s1", group = "B", timepoint = 1:100, values = stats::rnorm(100, sd = 50)))

  f <- calculate_features(tsibble::as_tsibble(d, key = c("id", "group"), index = "timepoint"),
                          feature_set = "tsfeatures", warn = FALSE)

  got <- f[f$names == "x_acf1", ]
  expect_equal(nrow(got), 2)

  for(g in c("A", "B")){
    expected <- unname(tsfeatures::acf_features(stats::ts(d$values[d$group == g]))["x_acf1"])
    expect_equal(got$values[got$group == g], expected)
  }
})

# Data where the first series (by key) starts later than the others, so the order in
# which index values first appear is not their sorted order

ragged_data <- function(){
  set.seed(10)
  rbind(data.frame(id = "a", timepoint = 5:104, values = stats::rnorm(100, sd = 1)),
        data.frame(id = "b", timepoint = 1:100, values = stats::rnorm(100, sd = 100)),
        data.frame(id = "c", timepoint = 1:100, values = stats::rnorm(100, sd = 10000)))
}


test_that("Kats receives each series in time order when series start at different times", {

  d <- ragged_data()

  # Stand-in for kats_calculator: like Kats, it sorts the values by their dates

  fake_source_python <- function(file, envir = parent.frame(), convert = TRUE){
    assign("kats_calculator", function(timepoints, values, warn){
      vals <- values[order(timepoints)]
      list(first_value = vals[1], last_value = vals[length(vals)])
    }, envir = envir)
  }

  local_mocked_bindings(source_python = fake_source_python, .package = "reticulate")

  f <- calculate_features(tsibble::as_tsibble(d, key = "id", index = "timepoint"),
                          feature_set = "kats", warn = FALSE)

  for(i in c("a", "b", "c")){
    x <- d$values[d$id == i]
    expect_equal(f$values[f$id == i & f$names == "first_value"], x[1])
    expect_equal(f$values[f$id == i & f$names == "last_value"], x[length(x)])
  }
})
