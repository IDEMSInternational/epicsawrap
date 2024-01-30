library(rpicsa)
library(dplyr)
library(testthat)

# Test case 
epicsadata::gcs_auth_file(file = "testdata/epicsa_token.json")
test_1_results <- readRDS("testdata/test_1_extreme_rain.rds")
test_2_results <- readRDS("testdata/test_2_extreme_tmin.rds")
test_3_results <- readRDS("testdata/test_3_extreme_tmax.rds")
country <- "zm"
station_id <- "test_1"

test_that("Correct summaries are calculated", {
  result <- extremes_rain(country, station_id)[[2]]
  expect_equal(nrow(result), nrow(test_1_results))
})

test_that("Correct summaries are calculated", {
  result <- extremes_tmin(country, station_id)[[2]]
  expect_equal(nrow(result), nrow(test_2_results))
})

test_that("Correct summaries are calculated", {
  result <- extremes_tmax(country, station_id)[[2]]
  expect_equal(nrow(result), nrow(test_3_results))
})
