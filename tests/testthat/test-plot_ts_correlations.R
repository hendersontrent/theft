context("test-plot_ts_correlations")

test_that("time series correlations", {
  expect_equal(length(unique(tmp$id)) ^ 2,
               nrow(plot_ts_correlations(tmp, 
                                         is_normalised = FALSE, 
                                         id_var = "id", 
                                         time_var = "timepoint", 
                                         values_var = "values", 
                                         method = "RobustSigmoid", 
                                         cor_method = "pearson", 
                                         interactive = FALSE)$data))
})
