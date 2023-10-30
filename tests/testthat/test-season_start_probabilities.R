library(testthat)
library(epicsawrap)

# Test case 1
epicsadata::gcs_auth_file(file = "e-picsa-e630400792e7.json")
test_1_results <- readRDS("testdata/test_1_season_start_probabilities.rds")
country <- "zm"
station_id <- "test_1"
test_that("Correct summaries are calculated", {
  result <- season_start_probabilities(country, station_id, c(0, 150, 300))
  expect_true(identical(result[[2]], test_1_results))
})
