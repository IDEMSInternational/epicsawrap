#' Probability Crop Tables
#' @description The probabilities of crop success for given planting maturity lengths, seasonal total rainfall requirements, and planting dates.
#'
#' @param country `character(1)` The country code of the data.
#' @param station_id `character` The id's of the stations to analyse. Either a
#'   single value or a vector.
#' @param water_requirements \code{numeric} Vector containing water requirements requirements.
#' @param planting_dates \code{numeric} Vector containing planting dates requirements.
#' @param planting_length \code{numeric} Vector containing seasonal crop length requirements.
#' @param start_before_season \code{logical} A logical value indicating whether to check the start day condition (default is `TRUE`).
#'
#' @return A list containing the definitions and a data frame with probability summaries.
#' @export
#'
#' @examples #
#' #library(epicsawrap)
#' #library(tidyverse)
#' #epicsawrap::setup(dir = getwd())
#' #epicsawrap::gcs_auth_file(file = "C:/Users/lclem/Downloads/e-picsa-e630400792e7.json")
#' #crop_success_probabilities(country = "zm", station_id = "16")
#' 
#' # or some can be defined in the dialog
#' #x <- crop_success_probabilities(country = "zm", station_id = "16", water_requirements = c(100, 300, 800))
#' 
#' # or all can be defined in the dialog
#' #crop_success_probabilities(country = "zm", station_id = "16", water_requirements = c(100, 300, 800),
#' #                           planting_length = c(100, 150), planting_dates = c(90, 100, 110))
crop_success_probabilities <- function(country,
                                        station_id,
                                        planting_dates = NULL,
                                        water_requirements = NULL,
                                        planting_length = NULL,
                                        start_before_season = NULL) {
  definitions <- epicsawrap::definitions(country = country, station_id = station_id, summaries = "crops_success")
  
  # Fetch daily data and preprocess
  daily <- epicsadata::get_daily_data(country = country, station_id = station_id)
  
  # For the variable names to be set as a certain default, set TRUE here, and run check_and_rename_variables
  data_names <- epicsadata::data_definitions(names(daily), TRUE)
  daily <- check_and_rename_variables(daily, data_names)
  
  season_data <- annual_rainfall_summaries(country = country, station_id = station_id, summaries = c("start_rains", "end_rains")) # end rains or end season?
  #offset <- season_data[[1]]$start_rains$s_start_doy
  
  check_and_set_parameter <- function(parameter_name, target_column_name) {
    if (is.null(get(parameter_name))) {
      value <- as.integer(definitions$crops_success[[target_column_name]])
      if (length(value) == 0) stop(paste(parameter_name, "parameter missing in definitions file."))
      assign(parameter_name, value, envir = parent.frame())
    } else {
      definitions$crops_success[[target_column_name]] <- get(parameter_name)
    }
  }
  check_and_set_parameter("planting_length", "planting_length")
  check_and_set_parameter("water_requirements", "water_requirements")
  check_and_set_parameter("planting_dates", "planting_dates")
  if (is.null(start_before_season)){
    start_before_season <- is.logical(definitions$crops_success$start_check)
    if (length(start_before_season) == 0) stop("start_before_season parameter missing in definitions file.")
  } else {
    definitions$crops_success$start_check <- start_before_season
  }
  
  summary_crops <- rpicsa::crops_definitions(data = daily,
                                             date_time  = data_names$date,
                                             station = data_names$station,
                                             year = data_names$year,
                                             rain = data_names$rain,
                                             water_requirements = as.integer(water_requirements),
                                             planting_dates = as.integer(planting_dates),
                                             planting_length = as.integer(planting_length),
                                             start_check = start_before_season,
                                             season_data = season_data[[2]],
                                             start_day = "start_rains_doy",
                                             end_day = "end_rains_doy")
  # convert all numeric to integers
  list_return <- NULL
  list_return[[1]] <- c(season_data[[1]], definitions)
  list_return[[2]] <- summary_crops
  return(list_return)
}
