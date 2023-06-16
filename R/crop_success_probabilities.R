#' Probability Crop Tables
#' 
#' @description The probabilities of crop success for given planting maturity lengths, seasonal total rainfall requirements, and planting dates. It is not required that the start of the rains occurs on or before the planting date.
#'
#' @param country `character(1)` The country code of the data.
#' @param station_id `character` The id's of the stations to analyse. Either a
#'   single value or a vector.
#' @param summaries `character` The names of the summaries to produce.
#'
#' @return A list containing the definitions and a data frame with probability summaries.
#' @export
#'
#' @examples # TODO
crop_success_probabilities <- function(country,
                                        station_id,
                                        summaries = c("crops_success")) {
  daily <- epicsadata::get_daily_data(country = country, station_id = station_id)
  if ("station_name" %in% names(daily)) daily$station <- daily$station_name # temp until we don't hard code in the columns call
  definitions <- epicsawrap::definitions(country = country, station_id = station_id, summaries = summaries)
  # TODO: call sor/eor if it is already run?
  season_data <- annual_rainfall_summaries(country = country, station_id = station_id, summaries = c("start_rains", "end_rains")) # end rains or end season?
  summary_crops <- rpicsa::crops_definitions(data = daily,
                                             date_time  = "date",
                                             station = "station",
                                             year = "year",
                                             rain = "rain",
                                             rain_totals = as.numeric(definitions$crops_success$rain_totals),
                                             plant_days = as.numeric(definitions$crops_success$plant_days),
                                             plant_lengths = as.numeric(definitions$crops_success$plant_lengths),
                                             start_check = is.logical(definitions$crops_success$start_check),
                                             season_data = season_data[[2]],
                                             start_day = "start_rain",
                                             end_day = "end_rain")
  list_return <- NULL
  list_return[[1]] <- c(definitions)
  list_return[[2]] <- summary_crops
  return(list_return)
}
