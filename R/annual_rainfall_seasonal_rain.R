#' Calculate Seasonal Rain
#'
#' This function calculates the seasonal rainfall based on the provided definitions, 
#' daily rainfall data, summary data, data variable names, and summary types.
#'
#' @param definitions A list containing definitions for various aspects of the analysis,
#'        including seasonal rainfall properties such as total rain, number of rainy days,
#'        and criteria for rainy days.
#' @param daily A data frame containing daily rainfall data.
#' @param summary_data A data frame containing summary data.
#' @param data_names A list of variable names used in the data frames.
#' @param summaries A character vector specifying the summary types.
#'
#' @return A numeric vector representing the seasonal rainfall based on the provided criteria.
#'
#' @export
#'
#' @examples
#' 
#'# # Example usage:
#'# definitions <- list(
#'#   seasonal_rain = list(
#'#     total_rain = TRUE,
#'#     n_rain = TRUE,
#'#     rain_day = 10,
#'#     na_rm = TRUE,
#'#     end_type = "rains"
#'#   )
#'# )
#'# data_names <- list(date = "date", station = "station", year = "year", rain = "rain")
#'# summary_types <- c("start_rains", "end_rains", "end_season")
#'# daily_data <- data.frame(date = seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "days"),
#'#                          station = "Station_A",
#'#                          year = rep(2023, each = 365),
#'#                          rain = runif(365, min = 0, max = 10))
#'# daily_data$doy <- lubridate::yday(daily_data$date)
#'# summary_data <- data.frame(year = 2023, start_rains = as.Date("2023-04-01"), 
#'#                            end_rains = as.Date("2023-10-31"), end_season = as.Date("2023-12-31"))
#'# seasonal_rain <- annual_rainfall_seasonal_rain(definitions, daily_data, summary_data, data_names, summary_types)
#'
annual_rainfall_seasonal_rain <- function(definitions, daily, summary_data, data_names, summaries) {
  if (is.null(definitions$seasonal_rain$total_rain)) 
    definitions$seasonal_rain$total_rain <- "FALSE"
  if (is.null(definitions$seasonal_rain$n_rain)) 
    definitions$seasonal_rain$n_rain <- "FALSE"
  if (definitions$seasonal_rain$n_rain){
    if (is.null(definitions$seasonal_rain$rain_day)) 
      stop("Missing value in seasonal_rain definitions for rain_day.")
    else
      definitions$seasonal_rain$rain_day <- NA
  }
  if (is.null(definitions$seasonal_rain$na_rm)){
    warning("Missing value in seasonal_rain definitions for na_rm. Setting na_rm = FALSE")
    definitions$seasonal_rain$na_rm <- "FALSE"
  }
  if (is.null(definitions$seasonal_rain$end_type)){
    present_values <- c("start_rains", "end_rains", "end_season") %in% summaries
    if ((length(present_values[present_values]) == 3) || (identical(present_values, c(TRUE, FALSE, TRUE)))) {
      warning("Performing seasonal_rain with end_season")
      end_date <- "end_season_date"
    }
    if (identical(present_values, c(TRUE, TRUE, FALSE))) {
      end_date <- "end_rains_date"
    }
  } else {
    if (definitions$seasonal_rain$end_type == "rains"){
      end_date <- "end_rains_date"
    } else {
      end_date <- "end_season_date"
    } 
  }
  if (!is.null(definitions$seasonal_rain$na_prop) && definitions$seasonal_rain$na_prop > 1){
    na_prop <- definitions$seasonal_rain$na_prop/100
  } else {
    na_prop <- NULL
  }
  season_rain <- rpicsa::seasonal_rain(
    summary_data = summary_data, 
    start_date = "start_rains_date", 
    end_date = end_date, 
    data = daily, 
    date_time = data_names$date, 
    station = data_names$station, 
    year = data_names$year, 
    rain = data_names$rain, 
    s_start_doy = definitions$start_rains$s_start_doy,
    total_rain = as.logical(definitions$seasonal_rain$total_rain), 
    n_rain = as.logical(definitions$seasonal_rain$n_rain), 
    rain_day = definitions$seasonal_rain$rain_day, 
    na_rm = as.logical(definitions$seasonal_rain$na_rm), 
    na_prop = na_prop, 
    na_n = definitions$seasonal_rain$na_n, 
    na_consec = definitions$seasonal_rain$na_consec, 
    na_n_non = definitions$seasonal_rain$na_n_non
  )
  return(season_rain)
}
