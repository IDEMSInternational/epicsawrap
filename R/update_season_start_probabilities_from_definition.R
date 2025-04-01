#' Update Season Start Probabilities from Definitions
#'
#' This function calculates the probabilities of the season starting on specified days based on the most recent definitions
#' for a given country and station. It uses rainfall data or start-of-rains summaries to compute the probabilities.
#'
#' @param country Character. The name of the country for which definitions and observations are retrieved.
#' @param station_id Character. The station ID(s) for which data is retrieved. Can be `NULL` if `definition_id` is specified. Defaults to `NULL`.
#' @param definition_id Character. The ID of the definitions to use for generating summaries. Only used if `station_id` is `NULL`. Defaults to `NULL`.
#' @param daily_data Data frame. (Optional) Daily rainfall data, required if `start_rains_data` is not provided. Defaults to `NULL`.
#' @param start_rains_data Data frame. (Optional) Precomputed start-of-rains data. If not provided, it is generated using `daily_data`. Defaults to `NULL`.
#'
#' @return A data frame containing the probabilities of the season starting on specified days, based on the definitions provided.
#' @export
#' 
#' @details
#' - The function retrieves definition data to identify the days of interest for calculating the start-of-season probabilities.
#' - If `start_rains_data` is not provided, it computes the start-of-rains summaries using `daily_data`.
#' - The function calculates probabilities for the specified days using the `rpicsa::probability_season_start` function.
#'
update_season_start_probabilities_from_definition <- function(country, station_id = NULL, definition_id = NULL, daily_data = NULL, start_rains_data = NULL) {
  if (!is.null(station_id) & !is.null(definition_id)) warning("Both station_id and definition_id are given. Defaulting to station_id.")
  # Retrieve the most recent definition data for the specified country and station
  if (!is.null(station_id)){
    definitions_data <- get_definitions_data(country = country, station_id = station_id)
  } else {
    definitions_data <- get_definitions_data(country = country, definitions_id = definition_id)
  }
  
  # If start-of-rains data is not provided, compute it using daily rainfall data
  if (is.null(start_rains_data)) {
    start_rains_data <- update_rainfall_summaries_from_definition(
      country = country, 
      station_id = station_id, 
      daily_data = daily_data, 
      summaries = "start_rains"
    )
  }
  
  # Extract the column names for the start-of-rains data
  data_names <- data_definitions(names(start_rains_data), FALSE, FALSE)
  
  # Retrieve the specified days for calculating season start probabilities
  start_dates <- as.numeric(definitions_data$season_start_probabilities$specified_day)
  if (length(start_dates) == 0) {
    warning("No specified days given for season start probability. No updates required.")
  }
  
  # Calculate season start probabilities
  summary_data <- rpicsa::probability_season_start(
    data = start_rains_data,
    station = data_names$station,
    start_rains = "start_rains_doy",
    doy_format = "doy_366", # Assuming day-of-year format is precomputed in start_rains summaries
    specified_day = as.integer(start_dates)
  )
  
  return(summary_data)
}