library(testthat)

# Test case 1
gcs_auth_file(file = "testdata/epicsa_token.json")
test_1_results <- readRDS("testdata/season_start_probabilities_r_data_test_1.rds")
country <- "internal_tests"
station_id <- "r_data_test_1"

test_that("Correct summaries are called", {
  result <- suppressWarnings(season_start_probabilities(country, station_id))
  expect_true(identical(names(result[[2]]), c("station", "day", "proportion")))
})

test_that("Correct summaries are calculated", {
  #result <- suppressWarnings(season_start_probabilities(country, station_id, call = "googlebuckets", override = TRUE))
  #expect_true(identical(result[[2]], test_1_results))
})
