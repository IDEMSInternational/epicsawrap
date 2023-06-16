#' Season start date probabilities
#' @description The probabilities of the start of the rains occurring on or before a set of specified days.
#'
#' @param country `character(1)` The country code of the data.
#' @param station_id `character` The id's of the stations to analyse. Either a
#'   single value or a vector.
#'
#' @return A list containing the definitions and a data frame with probability summaries.
#' @export
#'
#' @examples # TODO
season_start_probabilities <- function(country,
                                       station_id) {
  daily <- epicsadata::get_daily_data(country = country, station_id = station_id)
  if ("station_name" %in% names(daily)) daily$station <- daily$station_name # temp until we don't hard code in the columns call
  definitions <- epicsawrap::definitions(country = country, station_id = station_id, summaries = "season_start_probabilities")
  season_data <- annual_rainfall_summaries(country = country, station_id = station_id, summaries = c("start_rains"))
  summary_probabilities <- rpicsa::probability_season_start(data = season_data[[2]],
                              station = "station",
                              start_rains = "start_rain",
                              doy_format = "doy_366", # we calculate this in the start_rain summaries?
                              specified_day = as.numeric(definitions$season_start_probabilities$specified_day))
  list_return <- NULL
  list_return[[1]] <- c(season_data[[1]], definitions)
  list_return[[2]] <- summary_probabilities
  return(list_return)
}
