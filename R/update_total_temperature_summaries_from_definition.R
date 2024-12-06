#' Update Total Temperature Summaries from Definitions
#'
#' This function computes temperature summaries (e.g., minimum, maximum, mean) based on the most recent definitions 
#' for a specified country and station. It supports generating either annual or monthly summaries.
#'
#' @param country Character. The name of the country for which definitions and observations are retrieved. Defaults to "zm_workshops".
#' @param station_id Character. The station ID for which data is retrieved. Defaults to "Lundazi Met".
#' @param daily_data Data frame. Daily temperature data, including columns for station, date, year, tmin, and tmax.
#' @param to Character. Specifies whether the summaries should be generated for "annual" or "monthly" periods. Defaults to "annual".
#' 
#' @return A data frame containing temperature summaries based on the specified definitions and period (annual or monthly).
#' @export
update_total_temperature_summaries_from_definition <- function(country = "zm_workshops", station_id = "Lundazi Met", daily_data, to = c("annual", "monthly")) {
  to <- match.arg(to)
  
  # Retrieve the most recent definition data for the specified country and station
  definitions_data <- epicsawrap::get_definitions_data(country = country, station_id = station_id)
  
  data_names <- data_definitions(names(daily_data), FALSE, FALSE)
  
  # Initialise variables for storing summary data and summaries
  summary_data <- NULL
  summaries_list <- c("min_tmin", "mean_tmin", "max_tmin",
                      "min_tmax", "mean_tmax", "max_tmax")
  for (summary in summaries_list){
    definition_to <- unlist(definitions[[summary]]$to)
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
                                                             na_rm = as.logical(definitions[[summary]]$na_rm),
                                                             na_prop = definitions[[summary]]$na_prop,
                                                             na_n = definitions[[summary]]$na_n,
                                                             na_consec = definitions[[summary]]$na_consec,
                                                             na_n_non = definitions[[summary]]$na_n_non)    
    }
  }
  
  if (length(summary_data) > 1){
    summary_data <- Reduce(function(x, y) dplyr::full_join(x, y), summary_data)
  } else {
    summary_data <- summary_data[[1]]
  }
  summary_data$year <- as.integer(summary_data$year)
  if (!is.null(summary_data_monthly$month)) summary_data_monthly$month <- as.integer(forcats::as_factor(summary_data_monthly$month))
  return(summary_data) 
}

#' Update Annual Temperature Summaries from Definitions
#'
#' A wrapper function to generate only annual temperature summaries based on definitions.
#'
#' @param country Character. The name of the country for which definitions and observations are retrieved.
#' @param station_id Character. The station ID for which data is retrieved.
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
#' @param station_id Character. The station ID for which data is retrieved.
#' @param daily_data Data frame. Daily temperature data.
#' 
#' @return A data frame containing temperature summaries by month and year
#' @export
update_monthly_temperature_summaries_from_definition <- function(country, station_id, daily_data) {
  update_total_temperature_summaries_from_definition(country = country, station_id = station_id, daily_data = daily_data,
                                                     to = "monthly")
}
