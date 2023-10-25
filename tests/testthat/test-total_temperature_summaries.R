library(testthat)
library(epicsawrap)

# Test case 1
gcs_auth_file(file = "tests/e-picsa-e630400792e7.json")
test_1_annual <- readRDS("tests/test_1_annual.rds")
test_1_monthly <- readRDS("tests/test_1_monthly.rds")
country <- "zm"
station_id <- "test_1"

test_that("Correct summaries are calculated", {
  result_annual <- annual_temperature_summaries(country, station_id)
  result_monthly <- monthly_temperature_summaries(country, station_id)
  
  expect_true(identical(result_annual[[2]], test_1_annual))
  expect_true(identical(result_monthly[[2]], test_1_monthly))
})
