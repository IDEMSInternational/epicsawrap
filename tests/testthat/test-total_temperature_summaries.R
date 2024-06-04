library(testthat)

# Test case 1 
epicsadata::gcs_auth_file(file = "testdata/epicsa_token.json")
test_1_annual <- readRDS("testdata/annual_temperature_summaries_test_1.rds")
test_1_monthly <- readRDS("testdata/monthly_temperature_summaries_test_1.rds")
test_1_annual_narm <- readRDS("testdata/annual_temperature_summaries_test_1_narm_F.rds")
test_1_monthly_narm <- readRDS("testdata/monthly_temperature_summaries_test_1_narm_F.rds")
country <- "zm_test"
station_id <- "r_data_test_1"

test_that("Correct summaries are called", {
  result_annual <- suppressWarnings(annual_temperature_summaries(country, station_id, summaries = c("mean_tmin", "mean_tmax")))
  result_monthly <- suppressWarnings(monthly_temperature_summaries(country, station_id, summaries = c("mean_tmin", "mean_tmax")))
  
  expect_true(identical(data.frame(result_annual[[2]]), data.frame(test_1_annual)))
  expect_true(identical(data.frame(result_monthly[[2]]), data.frame(test_1_monthly)))
})

test_that("Correct summaries are calculated", {
  result_annual <- suppressWarnings(total_temperature_summaries(country, 
                                                                station_id, 
                                                                to = "annual",
                                                                summaries = c("mean_tmin", "mean_tmax"), 
                                                                override = TRUE))
  result_monthly <- suppressWarnings(total_temperature_summaries(country, 
                                                                 station_id, 
                                                                 to = "monthly",
                                                                 summaries = c("mean_tmin", "mean_tmax"), 
                                                                 override = TRUE))
  
  expect_true(identical(result_annual[[2]], test_1_annual_narm))
  expect_true(identical(result_monthly[[2]], test_1_monthly_narm))
})

test_that("Correct summaries are called", {
  result_annual <- suppressWarnings(total_temperature_summaries(country, station_id,
                                                                summaries = c("min_tmin"),
                                                                to = "annual",
                                                                override = TRUE))
  expect_true(identical(names(result_annual[[2]]), c("station", "year", "min_tmin")))
})
