#' Season start date probabilities
#' @description A table containing the probabilities of the season starting on or before a set of particular dates.
#'
#' @param country `character(1)` The country code of the data.
#' @param station_id `character` The id's of the stations to analyse. Either a
#'   single value or a vector.
#' @param start_dates `numeric` A vector of start dates (in doy format) to calculate the probabilities of the season starting on or before.
#' @param override A logical argument default `FALSE` indicating whether to calculate the summaries still, even if they are stored already in the bucket.
#'
#' @return A list containing the definitions and a data frame with probability summaries.
#' @export
#'
#' @examples #
#' #library(epicsawrap)
#' #library(tidyverse)
#' #epicsawrap::setup(dir = getwd())
#' #epicsawrap::gcs_auth_file(file = "C:/Users/lclem/Downloads/e-picsa-e630400792e7.json")
#' #season_start_probabilities(country = "zm", station_id = "16")
#' # or you can manually define
#' #season_start_probabilities(country = "zm", station_id = "16", start_dates = c(10, 20, 100))
season_start_probabilities <- function(country,
                                       station_id,
                                       start_dates = NULL,
                                       override = FALSE) {
  list_return <- NULL
  
  # do the summaries exist already?
  get_summaries <- epicsadata::get_summaries_data(country, station_id, summary = "season_start_probabilities")
  summary_data <- get_summaries[[1]]
  timestamp <- get_summaries[[2]]
  # what if the definitions is different? Have an override option.
  # if the summary data exists, and if you do not want to override it then:
  if (nrow(summary_data) > 0 & override == FALSE) {
    file_name <- epicsadata::get_objects_in_bucket(country, station_id, timestamp = timestamp)
    if (nrow(file_name) == 0) {
      list_return[[1]] <- (definitions(country, station_id, summaries = "season_start_probabilities"))
    } else {
      list_return[[1]] <- (definitions(country, station_id, summaries = "season_start_probabilities", paste0(station_id, ".", timestamp)))
    }
  } else {
    if (!is.null(timestamp)) file_name <- paste0(station_id, ".", timestamp)
    else file_name <- station_id
    definitions <- epicsawrap::definitions(country = country, station_id = station_id,
                                           summaries = "season_start_probabilities", file = file_name)
    # Fetch daily data and preprocess
    daily <- epicsadata::get_daily_data(country = country, station_id = station_id)
    
    # For the variable names to be set as a certain default, set TRUE here, and run check_and_rename_variables
    data_names <- epicsadata::data_definitions(names(daily), TRUE)
    daily <- check_and_rename_variables(daily, data_names)
    
    season_data <- annual_rainfall_summaries(country = country, station_id = station_id, summaries = c("start_rains"), override = override)
    if (is.null(start_dates)){
      start_dates <- definitions$season_start_probabilities$specified_day
      if (length(start_dates) == 0) stop("start_dates parameter missing in definitions file.")
    } else {
      definitions$season_start_probabilities$specified_day <- start_dates
    }
    
    summary_data <- rpicsa::probability_season_start(data = season_data[[2]],
                                                     station = data_names$station,
                                                     start_rains = "start_rains_doy",
                                                     doy_format = "doy_366", # we calculate this in the start_rains summaries?
                                                     specified_day = as.integer(start_dates))
    list_return[[1]] <- c(season_data[[1]], definitions)
  }
  list_return[[2]] <- summary_data
  return(list_return)
}
