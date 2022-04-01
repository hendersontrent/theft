context("test-plot_ts_correlations")

test_that("time series correlations", {
  expect_equal(length(unique(theft::simData$id)) ^ 2,
               nrow(plot_ts_correlations(theft::simData, 
                                         is_normalised = FALSE, 
                                         id_var = "id", 
                                         time_var = "timepoint", 
                                         values_var = "values", 
                                         method = "RobustSigmoid", 
                                         cor_method = "pearson", 
                                         interactive = FALSE)$data))
})
