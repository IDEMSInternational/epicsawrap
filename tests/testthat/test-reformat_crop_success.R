# library(testthat)
# library(dplyr)
# data <- data.frame(station_col = 1:5, total_rain_col = c(50, 60, 70, 80, 90),
#                    plant_day_col = c(100, 110, 120, 130, 140),
#                    plant_length_col = c(80, 90, 100, 110, 120),
#                    prop_success_col = c(0.6, 0.7, 0.8, 0.9, 1.0))
# 
# # Test for column renaming
# test_that("Columns are renamed correctly", {
#   reformatted_data <- reformat_crop_success(data,
#                                             station_col = "station_col", total_rain_col = "total_rain_col",
#                                             plant_day_col = "plant_day_col", plant_length_col = "plant_length_col",
#                                             prop_success_with_start_col = "prop_success_col")
#   
#   expect_equal(names(reformatted_data), c("station", "total_rain", "plant_day", "plant_length", "prop_success"))
# })
# 
# # Add more tests as needed
