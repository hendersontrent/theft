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

test_that("pyhctsa receives each series in time order and features are attached to the right keys", {

  d <- ragged_data()
  d$group <- unname(c(a = "A", b = "A", c = "B")[d$id])
  d$id[d$id == "c"] <- "b" # id "b" now appears in two groups

  # Stand-in for pyhctsa_calculator: returns the first value and length of each series

  fake_source_python <- function(file, envir = parent.frame(), convert = TRUE){
    assign("pyhctsa_calculator", function(timeseries, warn, config_yaml, n_jobs = 0){
      if(is.matrix(timeseries)){
        timeseries <- lapply(seq_len(nrow(timeseries)), function(i){timeseries[i, !is.na(timeseries[i, ])]})
      }
      data.frame(first_value = sapply(timeseries, function(x){x[1]}),
                 n = as.numeric(lengths(timeseries)))
    }, envir = envir)
  }

  local_mocked_bindings(source_python = fake_source_python, .package = "reticulate")

  f <- calculate_features(tsibble::as_tsibble(d, key = c("id", "group"), index = "timepoint"),
                          feature_set = "hctsa", warn = FALSE)

  expect_equal(nrow(f), 6)

  for(k in list(c("a", "A"), c("b", "A"), c("b", "B"))){
    x <- d$values[d$id == k[1] & d$group == k[2]]
    got <- f[f$id == k[1] & f$group == k[2], ]
    expect_equal(got$values[got$names == "first_value"], x[1])
    expect_equal(got$values[got$names == "n"], length(x))
  }
})

test_that("pyhctsa plumbing uses the tsibble's own key, index, and value names", {

  d <- data.frame(series = rep(c("u", "v"), each = 100), t = rep(1:100, 2), y = c(1:100, 101:200))

  fake_source_python <- function(file, envir = parent.frame(), convert = TRUE){
    assign("pyhctsa_calculator", function(timeseries, warn, config_yaml, n_jobs = 0){
      if(is.matrix(timeseries)){
        timeseries <- lapply(seq_len(nrow(timeseries)), function(i){timeseries[i, ]})
      }
      data.frame(first_value = sapply(timeseries, function(x){x[1]}))
    }, envir = envir)
  }

  local_mocked_bindings(source_python = fake_source_python, .package = "reticulate")

  f <- calculate_features(tsibble::as_tsibble(d, key = "series", index = "t"),
                          feature_set = "hctsa", warn = FALSE)

  expect_equal(f$values[f$id == "u"], 1)
  expect_equal(f$values[f$id == "v"], 101)
})

test_that("tsfresh features are attached to the right series", {

  skip_on_cran()
  skip_if_not(reticulate::py_module_available("tsfresh"), "tsfresh not available")

  pop_sd <- function(x){sqrt(mean((x - mean(x))^2))}

  # Ragged start times

  d <- ragged_data()
  f <- calculate_features(tsibble::as_tsibble(d, key = "id", index = "timepoint"),
                          feature_set = "tsfresh", warn = FALSE)

  for(i in c("a", "b", "c")){
    expect_equal(f$values[f$id == i & f$names == "values__standard_deviation"],
                 pop_sd(d$values[d$id == i]))
  }

  # The same id in two groups

  d$group <- unname(c(a = "A", b = "A", c = "B")[d$id])
  d$id[d$id == "c"] <- "b"
  d$timepoint[d$id == "a"] <- 1:100
  f <- calculate_features(tsibble::as_tsibble(d, key = c("id", "group"), index = "timepoint"),
                          feature_set = "tsfresh", warn = FALSE)

  expect_equal(nrow(f[f$names == "values__standard_deviation", ]), 3)

  for(k in list(c("a", "A"), c("b", "A"), c("b", "B"))){
    expect_equal(f$values[f$id == k[1] & f$group == k[2] & f$names == "values__standard_deviation"],
                 pop_sd(d$values[d$id == k[1] & d$group == k[2]]))
  }
})

test_that("tsfresh_cleanup = TRUE runs and keeps all series", {

  skip_on_cran()
  skip_if_not(reticulate::py_module_available("tsfresh"), "tsfresh not available")

  set.seed(10)
  d <- do.call(rbind, lapply(1:20, function(i){
    data.frame(id = paste0("s", i), group = ifelse(i <= 10, "A", "B"),
               timepoint = 1:100, values = stats::rnorm(100, sd = ifelse(i <= 10, 1, 5)))
  }))

  f <- calculate_features(tsibble::as_tsibble(d, key = c("id", "group"), index = "timepoint"),
                          feature_set = "tsfresh", tsfresh_cleanup = TRUE, warn = FALSE)

  expect_equal(nrow(unique(f[, c("id", "group")])), 20)
  expect_true("values__standard_deviation" %in% f$names)

  expect_error(calculate_features(tsibble::as_tsibble(d[, c("id", "timepoint", "values")], key = "id", index = "timepoint"),
                                  feature_set = "tsfresh", tsfresh_cleanup = TRUE, warn = FALSE),
               "requires a class label")
})
