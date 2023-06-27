#' Season start date probabilities
#' @description A table containing the probabilities of the season starting on or before a set of particular dates.
#'
#' @param country `character(1)` The country code of the data.
#' @param station_id `character` The id's of the stations to analyse. Either a
#'   single value or a vector.
#' @param start_dates `numeric` A vector of start dates (in doy format) to calculate the probabilities of the season starting on or before.
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
                                       start_dates = NULL) {
  daily <- epicsadata::get_daily_data(country = country, station_id = station_id)
  if ("station_name" %in% names(daily)) daily$station <- daily$station_name # temp until we don't hard code in the columns call
  definitions <- epicsawrap::definitions(country = country, station_id = station_id, summaries = "season_start_probabilities")
  season_data <- annual_rainfall_summaries(country = country, station_id = station_id, summaries = c("start_rains"))
  if (is.null(start_dates)){
    start_dates <- as.numeric(definitions$season_start_probabilities$specified_day)
  } else {
    definitions$season_start_probabilities$specified_day <- start_dates
  }
  summary_probabilities <- rpicsa::probability_season_start(data = season_data[[2]],
                                                              station = "station",
                                                              start_rains = "start_rain",
                                                              doy_format = "doy_366", # we calculate this in the start_rain summaries?
                                                              specified_day = start_dates)
  summary_probabilities <- summary_probabilities %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~as.integer(.)))
  list_return <- NULL
  list_return[[1]] <- c(season_data[[1]], definitions)
  list_return[[2]] <- summary_probabilities
  return(list_return)
}
