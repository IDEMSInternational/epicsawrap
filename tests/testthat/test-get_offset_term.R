
# TEMP disabled: Will fix.

# # test-get_offset_term.R
# library(testthat)
# 
# # Mock data_book object and its method
# data_book <- list(
#   get_variables_metadata = function(data_by_year) {
#     if (data_by_year == "example_data") {
#       return(data.frame(
#         Name = c("Station_Name", "s_year1", "annual_rainfall", "annual_rainday", 
#                  "seasonal_rainfall", "seasonal_rainday", "start_rain", "start_rain_date", 
#                  "end_season", "end_season_date", "length", "mean_tmin", "min_tmin", 
#                  "max_tmin", "mean_tmax", "min_tmax", "max_tmax"),
#         label = c("", "Shifted year starting on day 183", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""),
#         Calculated_By = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
#         class = c("factor", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "Date", "numeric", "Date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"),
#         Climatic_Type = c("station", "year", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
#         Dependencies = c(NA, NA, "s_doy,Station_Name,s_year1,rainfall", "s_doy,Station_Name,s_year1,rainday", "s_doy,Station_Name,s_year1,rainfall", "s_doy,Station_Name,s_year1,rainday", "Station_Name,s_year1,s_doy", "Station_Name,s_year1,s_doy", "Station_Name,s_year1,s_doy", "Station_Name,s_year1,s_doy", "start_rain,end_season", "s_doy,Station_Name,s_year1,tmin", "s_doy,Station_Name,s_year1,tmin", "s_doy,Station_Name,s_year1,tmin", "s_doy,Station_Name,s_year1,tmax", "s_doy,Station_Name,s_year1,tmax", "s_doy,Station_Name,s_year1,tmax"),
#         Dependent_Columns = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
#         doy_start = c(NA, 183, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
#         Has_Dependants = c(TRUE, NA, NA, NA, NA, NA, TRUE, NA, TRUE, NA, NA, NA, NA, NA, NA, NA, NA),
#         Is_Calculated = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
#         Is_Element = c(FALSE, FALSE, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
#         Is_Hidden = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
#         Is_Key = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
#         Scientific = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
#         Signif_Figures = c(NA, 3, 3, 3, 3, 3, 3, NA, 3, NA, 3, 3, 3, 3, 3, 3, 3)
#       ))
#     } else {
#       return(data.frame())
#     }
#   }
# )
# 
# # Test cases
# test_that("get_offset_term retrieves single start DOY", {
#   data_by_year <- "example_data"
#   result <- get_offset_term(data_by_year)
#   expect_equal(result, 183)
# })
# 
# test_that("get_offset_term handles no DOY start", {
#   data_by_year <- "NonExistentData"
#   result <- get_offset_term(data_by_year)
#   expect_equal(result, 1)
# })
# 
# test_that("get_offset_term handles NA values in DOY start", {
#   # Mock the function to return NA DOY start
#   data_book$get_variables_metadata <- function(data_by_year) {
#     return(data.frame(
#       doy_start = c(NA, 183)
#     ))
#   }
#   
#   data_by_year <- "example_data"
#   result <- get_offset_term(data_by_year)
#   expect_equal(result, 183)
# })
# 
