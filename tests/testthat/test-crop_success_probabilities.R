library(testthat)

# Test case 1
epicsadata::gcs_auth_file(file = "testdata/epicsa_token.json")
test_1_results <- readRDS("testdata/crop_success_probabilities_test_1.rds")
test_2_results <- readRDS("testdata/crop_success_probabilities_test_2.rds")
country <- "zm_test"
station_id <- "r_data_test_1"

test_that("Correct summaries are called", {
  result <- suppressWarnings(crop_success_probabilities(country, station_id))
  expect_true(identical(data.frame(result[[2]]), data.frame(test_1_results)))
})

test_that("Correct summaries are calculated", {
  result <- suppressWarnings(crop_success_probabilities(country, station_id, call = "googlebuckets", override = TRUE))
  expect_true(identical(result[[2]], test_2_results))
})

test_that("Recalculate summaries", {
  result <- suppressWarnings(crop_success_probabilities(country,
                                                        station_id = "zambia_eastern_4",
                                                        water_requirements = c(100, 300, 800),
                                                        planting_dates = 120,
                                                        planting_length = 120))
  expect_equal(nrow(result[[2]]), 15)
})


