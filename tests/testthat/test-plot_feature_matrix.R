context("test-plot_feature_matrix")

feature_matrix <- calculate_features(data = simData, id_var = "id", time_var = "timepoint", values_var = "values", group_var = "process", feature_set = "catch22", catch24 = FALSE)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

