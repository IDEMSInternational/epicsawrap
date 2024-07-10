#' Get Forecast Data
#'
#' @param country A character vector specifying the country or countries from which to get the forecast data. Options are defined in `get_bucket_name()`.
#' @param station_id A character string specifying the ID of the station for which to get the forecast data.
#'
#' @return todo
#' @export
#'
#' @examples # todo
get_forecast_data <- function(country, station_id) {
  # and anything else we need here! I've just taken this from update_daily_data.R
  filename <- paste0("data", "/", station_id, ".rds")
  saveto <- paste0(country, "/", filename)
  invisible(get_data(country = country, filename = filename, save_to = saveto))
}