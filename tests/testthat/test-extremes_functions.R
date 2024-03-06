library(rpicsa)
library(dplyr)
library(testthat)

# Test case 
epicsadata::gcs_auth_file(file = "testdata/epicsa_token.json")
test_1_results <- readRDS("testdata/extremes.rds")
country <- "zm"
station_id <- "test_1"

test_that("Correct summaries are calculated", {
  result <- extremes_summaries(country, station_id)[[2]]
  expect_equal(nrow(result), nrow(test_1_results))
})