library(testthat)
library(dplyr)

data <- data.frame(NAME = rep(1:3, each = 5),
                   YEAR = rep(2010:2014, each = 3),
                   planting_dates = rep(100:104, 3), planting_dates_condition = c(0, 1, 0, 1, 1))

# Test for column renaming
test_that("Columns are renamed correctly", {
  reformatted_data <- reformat_season_start(data, station_col = "NAME", year_col = "YEAR",
                                            plant_day_col = "planting_dates",
                                            plant_day_cond_col = "planting_dates_condition")
  
  expect_equal(names(reformatted_data), c("station", "day", "proportion"))
})

# Add more tests as needed
