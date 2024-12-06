#' Update Rainfall Summaries from Definitions
#'
#' This function generates summary data based on the most recent definitions and observed rainfall data
#' for a specified country and weather station. It calculates various metrics, such as start and end
#' of rains, seasonal length, seasonal rainfall, and annual rainfall, depending on the definitions provided.
#'
#' @param country Character. The name of the country for which definitions and observations are retrieved. Defaults to "zm_workshops".
#' @param station_id Character. The station ID for which data is retrieved. Defaults to "Lundazi Met".
#' @param daily_data Data frame. The daily data to update.
#' @param summaries `character` The names of the summaries to produce.
#' 
#' @return A data frame containing summarized rainfall data for the specified station and definitions.
#' @export
#' @examples
#' #update_rainfall_summaries_from_definition(country = "zm_workshops", station_id = "Lundazi Met")

update_rainfall_summaries_from_definition <- function(country = "zm_workshops", station_id = "Lundazi Met", daily_data,
                                                      summaries = c("annual_rain", "start_rains", "end_rains", "end_season", "seasonal_rain", "seasonal_length")) {
  # Retrieve the most recent definition data for the specified country and station
  definitions_data <- epicsawrap::get_definitions_data(country = country, station_id = station_id)
  
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
