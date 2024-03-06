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
#' #extremes_summaries(country, station_id, c("extremes_rain"))
extremes_summaries <- function(country, station_id,
                               summaries = c("extremes_rain", "extremes_tmin", "extremes_tmax")){
  # Fetch daily data and preprocess
  daily <- epicsadata::get_daily_data(country = country, station_id = station_id)
  # For the variable names to be set as a certain default, set TRUE here, and run check_and_rename_variables
  data_names <- epicsadata::data_definitions(names(daily), TRUE)
  daily <- check_and_rename_variables(daily, data_names)
  
  definitions <- epicsawrap::definitions(country = country, station_id = station_id, summaries = summaries)
  
  summary_data <- purrr::map(.x = summaries,
                             .f = ~ overall_extremes_summaries(daily = daily,
                                                               data_names = data_names, 
                                                               definitions = definitions,
                                                               summaries = .x))
  summary_data <- purrr::reduce(summary_data, dplyr::full_join)
  summary_data[is.na(summary_data)] <- 0
  return(list(definitions, summary_data))
}
