context("test-plot_ts_correlations")

test_that("time series correlations", {
  expect_equal(nrow(theft::simData),
               nrow(plot_ts_correlations(theft::simData, 
                                         is_normalised = FALSE, 
                                         id_var = "id", 
                                         time_var = "timepoint", 
                                         values_var = "values", 
                                         method = "RobustSigmoid", 
                                         cor_method = "pearson", 
                                         interactive = FALSE)$data))
})
