library(testthat)
library(rpicsa)

# Test case 1
epicsawrap::gcs_auth_file(file = "tests/e-picsa-e630400792e7.json")
test_1_results <- readRDS("tests/test_1_crop_success_probabilities.rds")
country <- "zm"
station_id <- "test_1"
result <- epicsawrap::crop_success_probabilities(country, station_id,
                                                 water_requirements = c(0, 100, 300))
test_that("Correct summaries are calculated", {
  expect_true(identical(result[[2]], test_1_results))
})