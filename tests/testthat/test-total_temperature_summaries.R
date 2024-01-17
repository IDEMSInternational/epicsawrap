library(testthat)
library(epicsadata)
library(epicsawrap)

# Test case 1
epicsadata::gcs_auth_file(file = "testdata/epicsa_token.json")
test_1_annual <- readRDS("testdata/test_1_annual.rds")
test_1_monthly <- readRDS("testdata/test_1_monthly.rds")
country <- "zm"
station_id <- "test_1"

test_that("Correct summaries are calculated", {
  result_annual <- annual_temperature_summaries(country, station_id, summaries = c("mean_tmin", "mean_tmax"))
  result_monthly <- monthly_temperature_summaries(country, station_id, summaries = c("mean_tmin", "mean_tmax"))
  
  expect_true(identical(result_annual[[2]], test_1_annual))
  expect_true(identical(result_monthly[[2]], test_1_monthly))
})
