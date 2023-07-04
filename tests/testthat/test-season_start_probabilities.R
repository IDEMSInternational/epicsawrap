# library(testthat)
# 
# # External functions call
# # Test case 1: Test with all parameters
# result <-
#   season_start_probabilities(
#     country = "zm",
#     station_id = "16",
#     start_dates = c(10, 20, 100)
#   )
# 
# # Test case 2: Test with missing parameters
# result_2 <-
#   season_start_probabilities(country = "zm", station_id = "16")
# 
# # Define test cases
# test_that("season_start_probabilities returns correct results", {
#   expect_no_error(result)
#   expect_no_error(result_2)
# })