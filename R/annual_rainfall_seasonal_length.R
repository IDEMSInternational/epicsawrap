#' Calculate Seasonal Length
#'
#' This function calculates the seasonal length of annual rainfall based on the
#' provided definitions, daily rainfall data, summary data, data variable names,
#' and summary types.
#'
#' @param definitions A list containing definitions for various aspects of the analysis.
#' @param daily A data frame containing daily rainfall data.
#' @param summary_data A data frame containing summary data.
#' @param data_names A list of variable names used in the data frames.
#' @param summaries A character vector specifying the summary types.
#'
#' @return A numeric vector representing the seasonal length of annual rainfall.
#'
#' @export
#'
#' @examples
#' 
#' # Example usage:
#' definitions <- list(seasonal_length = list(end_type = "rains"))
#' data_names <- list(date = "date", station = "station", year = "year", rain = "rain")
#' summary_types <- c("start_rains", "end_rains", "end_season")
#' daily_data <- data.frame(date = seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "days"),
#'                          station = "Station_A",
#'                          year = rep(2023, each = 365),
#'                          rain = runif(365, min = 0, max = 10))
#' summary_data <- data.frame(year = 2023, start_rains_date = as.Date("2023-04-01"), 
#'                            end_rains_date = as.Date("2023-10-31"), end_season_date = as.Date("2023-12-31"))
#' seasonal_length <- annual_rainfall_seasonal_length(definitions, daily_data, summary_data, data_names, summary_types)
#'
annual_rainfall_seasonal_length <- function(definitions, daily, summary_data, data_names, summaries) {
  if (is.null(definitions$seasonal_length$end_type)) {
    present_values <- c("start_rains", "end_rains", "end_season") %in% summaries
    if ((length(present_values[present_values]) == 3) || (identical(present_values, c(TRUE, FALSE, TRUE)))) {
      warning("Performing seasonal_length with end_season")
      end_date <- "end_season_date"
    }
    if (identical(present_values, c(TRUE, TRUE, FALSE))) end_date <- "end_rains_date"
  } else {
    if (definitions$seasonal_length$end_type == "rains") {
      end_date <- "end_rains_date"
    } else {
      end_date <- "end_season_date"
    } 
  }
  season_length <- rpicsa::seasonal_length(
    summary_data = summary_data, 
    start_date = "start_rains_date", 
    end_date = end_date, 
    data = daily, 
    date_time = data_names$date, 
    station = data_names$station, 
    year = data_names$year, 
    rain = data_names$rain
  )
  return(season_length)
}
