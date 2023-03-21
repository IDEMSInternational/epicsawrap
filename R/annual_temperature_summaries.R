#' Annual Temperature Summaries
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
#' # annual_temperature_summaries(country = "zm", station_id = "01122")
annual_temperature_summaries <- function(country,
                                         station_id,
                                         summaries = c("mean_tmin",
                                                       "mean_tmax")) {
  
  # TODO Once implemented
  # daily <- epicsadata::get_daily_data(country = country, 
  #                                     station_id = station_id)
  # annual <- rpicsa::annual_temperature(daily, date_time = "date", rain = "rain",
  #                                      year = "year", station = "station",
  #                                      na_rm = FALSE)
  
  annual <- data.frame(station_id = rep(station_id, each = 30),
                       year = rep(1991:2020, 2))
  annual$mean_tmin <- annual$year/5
  annual$mean_tmax <- annual$year/3
  return(annual)
}