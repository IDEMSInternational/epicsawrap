#' Calculate End of Rains
#'
#' This function calculates the end of the rainy season based on the provided definitions,
#' daily rainfall data, and data variable names.
#'
#' @param definitions A list containing definitions for the end of the rainy season analysis,
#'        including start and end days, interval length, and minimum rainfall criteria.
#' @param daily A data frame containing daily rainfall data.
#' @param data_names A list of variable names used in the data frame.
#'
#' @return A numeric vector representing the end of the rainy season in terms of day of the year (DOY).
#'
#' @export
#'
#' @examples
#' 
#' # Example usage:
#'# require(dplyr)
#'# library(cdms.products)
#'# data(daily_niger)
#'# definitions <- list(
#'#   end_rains = list(
#'#     start_day = 122,
#'#     end_day = 366,
#'#     interval_length = 7,
#'#     min_rainfall = 5
#'#   )
#'# )
#'# data_names <- list(date = "date", station = "station_name", year = "year", rain = "rain")
#'# daily_data <- daily_niger %>% dplyr::filter(year > 1975) %>% dplyr::filter(station_name == "Zinder")
#'# end_of_rains <- annual_rainfall_end_rains(definitions, daily_data, data_names)
#'
annual_rainfall_end_rains <- function(definitions, daily, data_names) {
  if (is.null(definitions$end_rains$start_day)) 
    stop("Missing value in end_rains definitions for start_day.")
  if (is.null(definitions$end_rains$end_day)) 
    stop("Missing value in end_rains definitions for end_day.")
  if (is.null(definitions$end_rains$interval_length)) 
    stop("Missing value in end_rains definitions for interval_length.")
  if (is.null(definitions$end_rains$min_rainfall)) 
    stop("Missing value in end_rains definitions for min_rainfall.")
  
  end_rains <- rpicsa::end_rains(
    daily, 
    date_time = data_names$date, 
    station = data_names$station, 
    year = data_names$year, 
    rain = data_names$rain,
    start_day = 1,#definitions$end_rains$start_day,
    end_day = definitions$end_rains$end_day,
    s_start_doy = definitions$end_rains$s_start_doy,
    output = "doy",
    interval_length = definitions$end_rains$interval_length,
    min_rainfall = definitions$end_rains$min_rainfall
  )
  
  # TODO: sort end_rains date because it stops at 366 at the moment.
  #end_rains$end_rains <- end_rains$end_rains + 100
  end_rains <- date_amendments(summary = "end_rains", data = end_rains, variable = end_rains, definitions)
  
  return(end_rains)
}
