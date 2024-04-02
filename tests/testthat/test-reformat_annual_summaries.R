library(testthat)
library(dplyr)

data <- data.frame(station_col = 1:5, year_col = 2010:2014,
                   start_rains_doy_col = 100:104, start_rains_date_col = c("2010-01-01", "2011-01-01", "2012-01-01", "2013-01-01", "2014-01-01"),
                   end_rains_doy_col = 200:204, end_rains_date_col = c("2010-12-31", "2011-12-31", "2012-12-31", "2013-12-31", "2014-12-31"),
                   end_season_doy_col = 300:304, end_season_date_col = c("2010-12-31", "2011-12-31", "2012-12-31", "2013-12-31", "2014-12-31"),
                   seasonal_rain_col = 50:54, n_seasonal_rain_col = 5:1,
                   season_length_col = c(90, 91, 92, 93, 94), annual_rain_col = c(500, 600, 700, 800, 900),
                   n_rain_col = c(50, 60, 70, 80, 90))

# Test for column renaming
test_that("Columns are renamed correctly", {
  reformatted_data <- reformat_annual_summaries(data,
                                                station_col = "station_col", year_col = "year_col",
                                                start_rains_doy_col = "start_rains_doy_col", start_rains_date_col = "start_rains_date_col",
                                                end_rains_doy_col = "end_rains_doy_col", end_rains_date_col = "end_rains_date_col",
                                                end_season_doy_col = "end_season_doy_col", end_season_date_col = "end_season_date_col",
                                                seasonal_rain_col = "seasonal_rain_col", n_seasonal_rain_col = "n_seasonal_rain_col",
                                                season_length_col = "season_length_col", annual_rain_col = "annual_rain_col",
                                                n_rain_col = "n_rain_col")
  
  expect_equal(names(reformatted_data), c("station", "year", "start_rains_doy", "start_rains_date",
                                          "end_rains_doy", "end_rains_date", "end_season_doy",
                                          "end_season_date", "seasonal_rain", "n_seasonal_rain",
                                          "season_length", "annual_rain", "n_rain"))
})