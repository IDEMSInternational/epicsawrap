# library(testthat)
# library(rpicsa)
# 
# # Test case 1
# country <- "zm"
# station_id <- "01122"
# summaries <- c("annual_rain")
# result <- epicsawrap::annual_rainfall_summaries(country, station_id, summaries)
# 
# country <- "zm"
# station_id_2 <- "16"
# summaries_2 <- c("start_rains", "end_rains", "annual_rain", "seasonal_rain")
# result_2 <- epicsawrap::annual_rainfall_summaries(country, station_id_2, summaries_2)
# 
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
#   expect_error(epicsawrap::annual_rainfall_summaries(country, station_id_2, c("end_rains", "seasonal_rain")))
#   
#   # Test case 3: Warning if both end are given and seasonal wanted
#   expect_warning(epicsawrap::annual_rainfall_summaries(country, station_id_2, c("start_rains", "end_rains", "end_season", "seasonal_rain")))
# })
