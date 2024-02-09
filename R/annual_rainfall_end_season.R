#' Calculate End of Season
#'
#' This function calculates the end of the rainy season based on the provided definitions,
#' daily rainfall data, and data variable names.
#'
#' @param definitions A list containing definitions for the end of the rainy season analysis,
#'        including start and end days, capacity, water balance, evaporation criteria,
#'        and related parameters.
#' @param daily A data frame containing daily rainfall data.
#' @param data_names A list of variable names used in the data frame.
#'
#' @return A numeric vector representing the end of the rainy season in terms of day of the year (DOY).
#'
#' @export
#'
#' @examples
#' # Example usage:
#'# require(dplyr)
#'# library(cdms.products)
#'# data(daily_niger)
#'# definitions <- list(
#'#   end_season = list(
#'#     start_day = 150,
#'#     end_day = 366,
#'#     capacity = 100,
#'#     water_balance_max = 50,
#'#     evaporation = "value",
#'#     evaporation_value = 10
#'#   )
#'# )
#'# data_names <- list(date = "date", station = "station_name", year = "year", rain = "rain")
#'# daily_data <- daily_niger %>% dplyr::filter(year > 1975) %>% dplyr::filter(station_name == "Zinder")
#'# end_of_season <- annual_rainfall_end_season(definitions, daily_data, data_names)
#'
annual_rainfall_end_season <- function(definitions, daily, data_names) {
  if (is.null(definitions$end_season$start_day)) 
    stop("Missing value in end_season definitions for start_day.")
  if (is.null(definitions$end_season$end_day)) 
    stop("Missing value in end_season definitions for end_day.")
  if (is.null(definitions$end_season$capacity)) 
    stop("Missing value in end_season definitions for capacity.")
  if (is.null(definitions$end_season$water_balance_max)) 
    stop("Missing value in end_season definitions for water_balance_max.")
  if (is.null(definitions$end_season$evaporation)) 
    stop("Missing value in end_season definitions for evaporation.")
  if (definitions$end_season$evaporation == "value"){
    if (is.null(definitions$end_season$evaporation_value)) 
      stop("Missing value in end_season definitions for evaporation_value.")
  }
  if (definitions$end_season$evaporation == "variable"){
    if (is.null(definitions$end_season$evaporation_variable)) 
      stop("Missing value in end_season definitions for evaporation_variable.")
  }
  
  end_season <- rpicsa::end_season(
    daily, 
    date_time = data_names$date, 
    station = data_names$station,
    year = data_names$year,
    rain = data_names$rain,
    start_day = definitions$end_season$start_day,
    end_day = definitions$end_season$end_day,
    s_start_doy = definitions$end_season$s_start_doy,
    output = "doy",
    capacity = definitions$end_season$capacity,
    water_balance_max = definitions$end_season$water_balance_max,
    evaporation = definitions$end_season$evaporation,
    evaporation_value = definitions$end_season$evaporation_value,
    evaporation_variable = definitions$end_season$evaporation_variable
  )
  
  end_season <- shift_year_amendments(summary = "end_season", data = end_season, variable = end_season, definitions)
  return(end_season)
}
