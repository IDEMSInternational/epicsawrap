# library(testthat)
# 
# # Test case 1: Test with to = "annual"
# result <- total_temperature_summaries(
#   country = "zm",
#   station_id = "23",
#   summaries = c("mean_tmin", "mean_tmax"),
#   to = "annual"
# )
# 
# # Test case 2: Test with to = "monthly"
# result_2 <- total_temperature_summaries(
#   country = "zm",
#   station_id = "1",
#   summaries = c("mean_tmin", "mean_tmax"),
#   to = "monthly"
# )
# 
# # Define test cases
# test_that("total_temperature_summaries returns correct results", {
#   # Mock the necessary external functions or datasets if needed
#   expect_length(result, 2)
#   expect_length(result_2, 2)
#   
#   expect_no_error(result)
#   expect_no_error(result_2)
# })