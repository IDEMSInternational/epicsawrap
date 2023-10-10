# library(testthat)
# 
# # External functions call
# # Test case 1: Test with all parameter
# result <- epicsawrap::crop_success_probabilities(
#   country = "zm",
#   station_id = "16",
#   planting_dates = c(92, 122, 153),
#   water_requirements = c(300, 500, 700),
#   planting_length = c(120, 180),
#   start_before_season = TRUE
# )
# 
# # Test case 2: Test with missing planting_length parameter
# result_2 <- epicsawrap::crop_success_probabilities(
#   country = "zm",
#   station_id = "16",
#   planting_dates = c(1, 2, 3),
#   water_requirements = c(100, 200, 300),
#   start_before_season = TRUE
# )
# 
# # Test case 2: Test with all missing parameters
# result_3 <- epicsawrap::crop_success_probabilities(
#   country = "zm",
#   station_id = "16"
# )
# 
# # Define test cases
# test_that("crop_success_probabilities returns correct results", {
#   expect_no_error(result)
#   expect_no_error(result_2)
#   expect_no_error(result_3)
# })
# 
# # Define test cases
# test_that("crop_success_probabilities returns same results whether parameters are defined or not", {
#   expect_identical(result[[2]], result_3[[2]])
# })
# 
