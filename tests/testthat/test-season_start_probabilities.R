library(testthat)
library(epicsadata)
library(epicsawrap)

# Test case 1
epicsadata::gcs_auth_file(file = "testdata/epicsa_token.json")
test_1_results <- readRDS("testdata/season_start_probabilities_test_1.rds")
country <- "zm"
station_id <- "test_1"

test_that("Correct summaries are called", {
  result <- season_start_probabilities(country, station_id, c(0, 150, 300))
  expect_true(identical(result[[2]], test_1_results))
})

test_that("Correct summaries are calculated", {
  result <- season_start_probabilities(country, station_id, c(0, 150, 300), override = TRUE)
  expect_true(identical(result[[2]], test_1_results))
})
