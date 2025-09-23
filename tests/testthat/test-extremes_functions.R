# library(dplyr)
# library(testthat)

# # fix up when we've added extremes into new system. this is not working at the moment.

# # Test case 
# gcs_auth_file(file = "testdata/epicsa_token.json")
# test_1_results <- readRDS("testdata/extremes_summaries_test_1.rds")
# country <- "zm"
# station_id <- "test_1"

# # test_that("Correct summaries are called", {
# #   result <- suppressWarnings(extremes_summaries(country, station_id, override = TRUE))[[2]]
# #   expect_equal(nrow(result), nrow(test_1_results))
# # })

# # test_that("Correct summaries are calculated", {
# #   result <- suppressWarnings(extremes_summaries(country, station_id, override = TRUE)[[2]])
# #   expect_equal(nrow(result), nrow(test_1_results))
# # })
