context("test-process_hctsa_file")

d <- R.matlab::readMat("https://cloudstor.aarnet.edu.au/plus/s/6sRD6IPMJyZLNlN/download")

test_that("processing hctsa file", {
  expect_equal(length(d$timeSeriesData)* length(d$timeSeriesData[[1]][[1]]), 
               nrow(process_hctsa_file("https://cloudstor.aarnet.edu.au/plus/s/6sRD6IPMJyZLNlN/download")))
})
