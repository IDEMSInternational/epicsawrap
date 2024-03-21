library(testthat)
library(epicsadata)
library(epicsawrap)

# Test case 1
epicsadata::gcs_auth_file(file = "testdata/epicsa_token.json")
test_1_results <- readRDS("testdata/test_1_annual_rainfall_summaries.rds")
country <- "zm"
station_id <- "test_1"

test_that("Correct summaries are calculated", {
  result <- annual_rainfall_summaries(country, station_id, override = TRUE)
  expect_true(identical(result[[2]], test_1_results))
})

# test_that("Correct summaries are called", {
#   result <- annual_rainfall_summaries1(country, station_id, override = FALSE)
#   expect_true(identical(result[[2]], test_1_results))
# })

# test_that("Correct summaries are calculated", {
#   # Test case 2
#   expect_no_error(result)
#   # Test case 3 (add more test cases if needed)
#   expect_no_error(result_2)
# })
# 
# test_that("Error is thrown if undefined values", {
#   # Test case 1: Error is thrown if undefined values
#   expect_error(epicsawrap::annual_rainfall_summaries(country, station_id, "end_season"))
# 
#   # Test case 2: Error is thrown if start_ is not calculated but needed
#   expect_error(epicsawrap::annual_rainfall_summaries(country, station_id, c("seasonal_rain")))
# 
#   # Test case 3: Warning if both end are given and seasonal wanted
#   expect_warning(epicsawrap::annual_rainfall_summaries(country, station_id_2, c("start_rains", "end_rains", "end_season", "seasonal_rain")))
# })
