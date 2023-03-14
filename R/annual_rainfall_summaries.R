#' Annual Rainfall Summaries
#'
#' @param country `character(1)` The country code of the data.
#' @param station_id `character` The id's of the stations to analyse. Either a
#'   single value or a vector.
#' @param summaries `character` The names of the summaries to produce.
#'
#' @return A data frame with yearly summaries.
#' @export
#'
#' @examples
#' # annual_rainfall_summaries(country = "zm", station_id = "01122")
annual_rainfall_summaries <- function(country,
                                      station_id, 
                                      summaries = c("seasonal_rainfall",
                                                    "seasonal_raindays",
                                                    "start_rains",
                                                    "end_season",
                                                    "length_season")) {
  daily <- epicsadata::get_daily_data(country = country, 
                                      station_id = station_id)
  annual <- rpicsa::annual_rain(daily, date_time = "date", rain = "rain",
                                year = "year", station = "station",
                                na_rm = FALSE)
  return(annual)
}