#' Calculate Annual Rainfall
#'
#' This function calculates annual rainfall based on the provided definitions,
#' daily rainfall data, and data variable names.
#'
#' @param definitions A list containing definitions for the annual rainfall analysis,
#'        including options for calculating total rainfall, number of rainy days,
#'        and criteria for handling missing values.
#' @param daily A data frame containing daily rainfall data.
#' @param data_names A list of variable names used in the data frame.
#'
#' @return A numeric vector representing annual rainfall and related calculations.
#'
#' @export
#'
#' @examples
#' 
#'# # Example usage:
#'# require(dplyr)
#'# library(cdms.products)
#'# data(daily_niger)
#'# definitions <- list(
#'#   annual_rain = list(
#'#     annual_rain = TRUE,
#'#     n_rain = FALSE,
#'#     na_rm = TRUE
#'#   )
#'# )
#'# data_names <- list(date = "date", station = "station_name", year = "year", rain = "rain")
#'# daily_data <- daily_niger %>% dplyr::filter(year > 1975) %>% dplyr::filter(station_name == "Zinder")
#'# annual_rainfall <- annual_rainfall_annual_rain(definitions, daily_data, data_names)
#'
annual_rainfall_annual_rain <- function(definitions, daily, data_names) {
  if (is.null(definitions$annual_rain$annual_rain)) 
    definitions$annual_rain$annual_rain <- "FALSE"
  if (definitions$annual_rain$n_rain){
    if (is.null(definitions$annual_rain$rain_day)) 
      stop("Missing value in annual_rain definitions for rain_day.")
    else
      definitions$annual_rain$rain_day <- NA
  }
  if (is.null(definitions$annual_rain$na_rm)){
    warning("Missing value in annual_rain definitions for na_rm. Setting na_rm = FALSE")
    definitions$annual_rain$na_rm <- "FALSE"
  }
  if (!is.null(na_prop = definitions$annual_rain$na_prop) && definitions$annual_rain$na_prop > 1){
    na_prop <- definitions$annual_rain$na_prop/100
  } else {
    na_prop <- NULL
  }
  annual_rain <- rpicsa::annual_rain(
    daily, 
    date_time = data_names$date, 
    rain = data_names$rain, 
    year = data_names$year, 
    station = data_names$station, 
    total_rain = as.logical(definitions$annual_rain$annual_rain),
    n_rain = as.logical(definitions$annual_rain$n_rain),
    rain_day = definitions$annual_rain$rain_day,
    na_rm = as.logical(definitions$annual_rain$na_rm),
    na_prop = na_prop,
    na_n = definitions$annual_rain$na_n,
    na_consec = definitions$annual_rain$na_consec,
    na_n_non = definitions$annual_rain$na_n_non
  )
  return(annual_rain)
}