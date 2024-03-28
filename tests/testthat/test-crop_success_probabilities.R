library(testthat)

# Test case 1
epicsadata::gcs_auth_file(file = "testdata/epicsa_token.json")
test_1_results <- readRDS("testdata/crop_success_probabilities_test_1.rds")
country <- "zm"
station_id <- "test_1"

test_that("Correct summaries are called", {
  result <- crop_success_probabilities(country, station_id, water_requirements = c(0, 100, 300))
  expect_true(identical(result[[2]], test_1_results))
})

test_that("Correct summaries are calculated", {
  result <- crop_success_probabilities(country, station_id, water_requirements = c(0, 100, 300), override = TRUE)
  expect_true(identical(result[[2]], test_1_results))
})