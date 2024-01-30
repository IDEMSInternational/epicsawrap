#' Get the Extreme Data
#'
#' This function identifies extreme values in a specified element (column) of a data frame. It can operate in two modes: percentile-based and threshold-based.
#' 
#' @param country A character string specifying the country code of the data.
#' @param station_id A character vector specifying the ID(s) of the station(s) to analyse.
#' @param summaries A character vector specifying the names of the summaries to produce.
#'
#' @return A data frame containing the extreme data.
#' 
#' @examples
#' # Generate annual temperature summaries for station 16 in Zambia
#' #overall_extremes_summaries(country, station_id, c("extremes_rain"), to = "annual")
overall_extremes_summaries <- function(country, station_id,
                                       summaries = c("extremes_rain", "extremes_tmin", "extremes_tmax")){
  summaries <- match.arg(summaries)
  daily <- epicsadata::get_daily_data(country = country, station_id = station_id)
  definitions <- epicsawrap::definitions(country = country, station_id = station_id, summaries = summaries)
  data_names <- epicsadata::data_definitions(station_id = station_id)
  
  if (is.null(definitions[[summaries]]$direction)){
    direction <- "greater"
  } else {
    direction <- as.character(definitions[[summaries]]$direction)
  }
  if (is.null(definitions[[summaries]]$type)){
    type <- "greater"
  } else {
    type <- as.character(definitions[[summaries]]$type)
  }
  summary_data <- rpicsa::get_extremes(data = daily,
                                        element = if (summaries == "extremes_rain") data_names$rain else if (summaries == "extremes_tmin") data_names$tmin else if (summaries == "extremes_tmax") data_names$tmax else NULL,
                                        type = type,
                                        direction = direction,
                                        value = as.integer(definitions[[summaries]]$value))
  
  summary_data$year <- as.integer(summary_data$year)
  if ("month" %in% names(summary_data)) {
    summary_data$month <- as.integer(forcats::as_factor(summary_data$month))
  }
  
  return(list(definitions, summary_data))
}
