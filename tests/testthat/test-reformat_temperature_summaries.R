library(testthat)
library(dplyr)

data <- data.frame(station_col = 1:5, year_col = 2010:2014, month_col = 1:5,
                   mean_tmin_col = c(10, 11, 12, 13, 14), min_tmin_col = c(5, 6, 7, 8, 9),
                   max_tmin_col = c(15, 16, 17, 18, 19), mean_tmax_col = c(20, 21, 22, 23, 24),
                   min_tmax_col = c(15, 16, 17, 18, 19), max_tmax_col = c(25, 26, 27, 28, 29))

# Test for column renaming
test_that("Columns are renamed correctly", {
  reformatted_data <- reformat_temperature_summaries(data,
                                                     station_col = "station_col", year_col = "year_col", month_col = "month_col",
                                                     mean_tmin_col = "mean_tmin_col", min_tmin_col = "min_tmin_col", max_tmin_col = "max_tmin_col",
                                                     mean_tmax_col = "mean_tmax_col", min_tmax_col = "min_tmax_col", max_tmax_col = "max_tmax_col")
  
  expect_equal(names(reformatted_data), c("station", "year", "month", "mean_tmin", "min_tmin", "max_tmin", "mean_tmax", "min_tmax", "max_tmax"))
})

# Add more tests as needed
