#' Update Rainfall Summaries from Definitions
#'
#' This function generates rainfall summary data based on the most recent definitions and observed rainfall data
#' for a specified country. Summaries include metrics such as start and end of rains, seasonal length, seasonal rainfall, 
#' and annual rainfall, depending on the definitions and specified summary types.
#'
#' @param country Character. The name of the country for which definitions and observations are retrieved. Defaults to `"zm_workshops"`.
#' @param station_id Character. The station ID(s) for which data is retrieved. Can be `NULL` if `definition_id` is specified. Defaults to `NULL`.
#' @param definition_id Character. The ID of the definitions to use for generating summaries. Only used if `station_id` is `NULL`. Defaults to `NULL`.
#' @param daily_data Data frame. The daily rainfall data used to generate summaries.
#' @param summaries Character vector. The names of the summaries to produce. Options include:
#'   - `"annual_rain"`: Calculates total annual rainfall.
#'   - `"start_rains"`: Identifies the start of the rainy season.
#'   - `"end_rains"`: Identifies the end of the rainy season.
#'   - `"end_season"`: Identifies the end of the season.
#'   - `"seasonal_rain"`: Calculates total seasonal rainfall.
#'   - `"seasonal_length"`: Calculates the length of the rainy season.
#'   Defaults to all available summaries.
#' 
#' @return A data frame containing the requested rainfall summaries for the specified station(s) or definitions.
#'
#' @details
#' - If `station_id` is provided, the function retrieves the corresponding definitions data for the station.
#' - If `station_id` is `NULL`, the function fetches definitions data directly using the `definition_id`.
#' - The function only calculates the summaries specified in the `summaries` argument.
#' - Summary calculations depend on definitions being available for the requested metrics.
#'
#' @export
update_rainfall_summaries_from_definition <- function(country = "zm_workshops", station_id = NULL, definition_id = NULL, daily_data,
                                                      summaries = c("annual_rain", "start_rains", "end_rains", "end_season", "seasonal_rain", "seasonal_length")) {
  if (!is.null(station_id) & !is.null(definition_id)) warning("Both station_id and definition_id are given. Defaulting to station_id.")
  # Retrieve the most recent definition data for the specified country and station
  if (!is.null(station_id)){
    definitions_data <- get_definitions_data(country = country, station_id = station_id)
  } else {
    definitions_data <- get_definitions_data(country = country, definition_id = definition_id)
  }
  
  # Initialize variables for storing summary data and summaries
  summary_data <- NULL
  
  # Calculate start of rains if the definition exists
  if ("start_rains" %in% summaries){
    if (!is.null(definitions_data$start_rains$end_day)) {
      start_rains <- annual_rainfall_start_rains(
        definitions = definitions_data,
        daily = daily_data,
        data_names = data_definitions(names(daily_data), FALSE, FALSE)
      )
      summary_data <- join_null_data(summary_data, start_rains)
      summary_data$start_rains_doy <- as.integer(summary_data$start_rains_doy)
    }
  }
  
  # Calculate end of rains if the definition exists
  if ("end_rains" %in% summaries) {
    if (!is.null(definitions_data$end_rains$end_day)) {
      end_rains <- annual_rainfall_end_rains(
        definitions = definitions_data,
        daily = daily_data,
        data_names = data_definitions(names(daily_data), FALSE, FALSE)
      )
      summary_data <- join_null_data(summary_data, end_rains)
      summary_data$end_rains_doy <- as.integer(summary_data$end_rains_doy)
    }
  }
  
  # Calculate end of the rainy season if the definition exists
  if ("end_season" %in% summaries) {
    if (!is.null(definitions_data$end_season$end_day)) {
      end_season <- annual_rainfall_end_season(
        definitions = definitions_data,
        daily = daily_data,
        data_names = data_definitions(names(daily_data), FALSE, FALSE)
      )
      summary_data <- join_null_data(summary_data, end_season)
      summary_data$end_season_doy <- as.integer(summary_data$end_season_doy)
    }
  }
  
  
  if ("seasonal_length" %in% summaries) {
    # Calculate seasonal length if summary data is available
    if (!is.null(summary_data)) {
      seasonal_length <- annual_rainfall_seasonal_length(
        definitions = definitions_data,
        summary_data = summary_data,
        daily = daily_data,
        data_names = data_definitions(names(daily_data), FALSE, FALSE)
      )
      summary_data <- join_null_data(summary_data, seasonal_length)
    }
  }
  
  if ("seasonal_rain" %in% summaries) {
    # Calculate seasonal rainfall if definitions exist and conditions are met
    if (!is.null(summary_data) && 
        !is.null(definitions_data$seasonal_rain$total_rain) && 
        (definitions_data$seasonal_rain$total_rain == "TRUE" || 
         definitions_data$seasonal_rain$n_rain == "TRUE")) {
      seasonal_rain <- annual_rainfall_seasonal_rain(
        definitions = definitions_data,
        summary_data = summary_data,
        daily = daily_data,
        summaries = summaries,
        data_names = data_definitions(names(daily_data), FALSE, FALSE)
      )
      summary_data <- join_null_data(summary_data, seasonal_rain)
    }
  }
  
  if ("annual_rain" %in% summaries) {
    # Calculate annual rainfall if definitions exist and conditions are met
    if (!is.null(definitions_data$annual_rain$total_rain) && 
        (definitions_data$annual_rain$total_rain == "TRUE" || 
         definitions_data$annual_rain$n_rain == "TRUE")) {
      annual_rain <- annual_rainfall_annual_rain(
        definitions = definitions_data,
        daily = daily_data,
        data_names = data_definitions(names(daily_data), FALSE, FALSE)
      )
      summary_data <- join_null_data(summary_data, annual_rain)
    }
  }
  
  # Return the summary data
  return(summary_data)
}
