#' Update Total Temperature Summaries from Definitions
#'
#' This function computes temperature summaries (e.g., minimum, maximum, mean) based on the most recent definitions 
#' for a specified country and station. It supports generating either annual or monthly summaries.
#'
#' @param country Character. The name of the country for which definitions and observations are retrieved. Defaults to "zm_workshops".
#' @param station_id Character. The station ID(s) for which data is retrieved. Can be `NULL` if `definition_id` is specified. Defaults to `NULL`.
#' @param definition_id Character. The ID of the definitions to use for generating summaries. Only used if `station_id` is `NULL`. Defaults to `NULL`.
#' @param daily_data Data frame. Daily temperature data, including columns for station, date, year, tmin, and tmax.
#' @param to Character. Specifies whether the summaries should be generated for "annual" or "monthly" periods. Defaults to "annual".
#' 
#' @return A data frame containing temperature summaries based on the specified definitions and period (annual or monthly).
#' @export
update_total_temperature_summaries_from_definition <- function(country = "zm_workshops", station_id = NULL, definition_id = NULL, daily_data, to = c("annual", "monthly")) {
  to <- match.arg(to)
  
  # Retrieve the most recent definition data for the specified country and station
  if (!is.null(station_id) & !is.null(definition_id)) warning("Both station_id and definition_id are given. Defaulting to station_id.")
  if (!is.null(station_id)){
    definitions_data <- get_definitions_data(country = country, station_id = station_id)
  } else {
    definitions_data <- get_definitions_data(country = country, definitions_id = definition_id)
  }
  
  data_names <- data_definitions(names(daily_data), FALSE, FALSE)
  
  # Initialise variables for storing summary data and summaries
  summary_data <- NULL
  summaries_list <- c("min_tmin", "mean_tmin", "max_tmin",
                      "min_tmax", "mean_tmax", "max_tmax")
  for (summary in summaries_list){
    definition_to <- unlist(definitions_data[[summary]]$to)
    summary_type <- gsub("_.*$", "", summary)
    summary_variable <- gsub("^.*_", "", summary)
    
    if (to %in% definition_to){
      summary_data[[summary]] <- rpicsa::summary_temperature(data = daily_data,
                                                             date_time = data_names$date,
                                                             station = data_names$station,
                                                             year = data_names$year,
                                                             tmax = if (summary_variable == "tmax") data_names$tmax else NULL,
                                                             tmin = if (summary_variable == "tmin") data_names$tmin else NULL,
                                                             summaries = summary_type,
                                                             to = to,
                                                             na_rm = as.logical(definitions_data[[summary]]$na_rm),
                                                             na_prop = definitions_data[[summary]]$na_prop,
                                                             na_n = definitions_data[[summary]]$na_n,
                                                             na_consec = definitions_data[[summary]]$na_consec,
                                                             na_n_non = definitions_data[[summary]]$na_n_non)    
    }
  }
  
  print(length(summary_data))
  
  if (length(summary_data) > 1){
    summary_data <- Reduce(function(x, y) dplyr::full_join(x, y), summary_data)
  } else {
    summary_data <- summary_data[[1]]
  }

  #summary_data$year <- as.integer(summary_data$year)
  if (!is.null(summary_data$month)) summary_data$month <- as.integer(forcats::as_factor(summary_data$month))
  return(summary_data) 
}

#' Update Annual Temperature Summaries from Definitions
#'
#' A wrapper function to generate only annual temperature summaries based on definitions.
#'
#' @param country Character. The name of the country for which definitions and observations are retrieved.
#' @param station_id Character. The station ID(s) for which data is retrieved. Can be `NULL` if `definition_id` is specified. Defaults to `NULL`.
#' @param definition_id Character. The ID of the definitions to use for generating summaries. Only used if `station_id` is `NULL`. Defaults to `NULL`.
#' @param daily_data Data frame. Daily temperature data.
#' 
#' @return A data frame containing annual temperature summaries.
#' @export
update_annual_temperature_summaries_from_definition <- function(country, station_id, daily_data) {
  update_total_temperature_summaries_from_definition(country = country, station_id = station_id, daily_data = daily_data,
                                                     to = "annual")
}
#' Update Monthly Temperature Summaries from Definitions
#'
#' A wrapper function to generate temperature summaries by month and year based on definitions.
#'
#' @param country Character. The name of the country for which definitions and observations are retrieved.
#' @param station_id Character. The station ID(s) for which data is retrieved. Can be `NULL` if `definition_id` is specified. Defaults to `NULL`.
#' @param definition_id Character. The ID of the definitions to use for generating summaries. Only used if `station_id` is `NULL`. Defaults to `NULL`.
#' @param daily_data Data frame. Daily temperature data.
#' 
#' @return A data frame containing temperature summaries by month and year
#' @export
update_monthly_temperature_summaries_from_definition <- function(country, station_id, daily_data) {
  update_total_temperature_summaries_from_definition(country = country, station_id = station_id, daily_data = daily_data,
                                                     to = "monthly")
}
