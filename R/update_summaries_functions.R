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
    definitions_data <- get_definitions_data(country = country, definitions_id = definition_id)
  }
  
  # Initialize variables for storing summary data and summaries
  summary_data <- NULL
  
  data_names <- data_definitions(names(daily_data), FALSE, FALSE)
  # Calculate start of rains if the definition exists
  if ("start_rains" %in% summaries){
    if (!is.null(definitions_data$start_rains$end_day)) {
      start_rains <- annual_rainfall_start_rains(
        definitions = definitions_data,
        daily = daily_data,
        data_names = data_names
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
        data_names = data_names
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
        data_names = data_names
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
        data_names = data_names
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
        data_names = data_names
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
        data_names = data_names
      )
      if (is.factor(summary_data[[data_names$year]])){
        if (!is.factor(annual_rain[[data_names$year]])) annual_rain[[data_names$year]] <- factor(annual_rain[[data_names$year]])
      }
      summary_data <- join_null_data(summary_data, annual_rain)
    }
  }
  
  # Return the summary data
  return(summary_data)
}

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
#' @param daily_data Data frame. Daily temperature data.
#' 
#' @return A data frame containing temperature summaries by month and year
#' @export
update_monthly_temperature_summaries_from_definition <- function(country, station_id, daily_data) {
  update_total_temperature_summaries_from_definition(country = country, station_id = station_id, daily_data = daily_data,
                                                     to = "monthly")
}

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
#' - The function calculates probabilities for the specified days using the `probability_season_start` function in `rpicsa`
#'
update_season_start_probabilities_from_definition <- function(country, station_id = NULL, definition_id = NULL, daily_data = NULL, start_rains_data = NULL) {
  # if (!is.null(station_id) & !is.null(definition_id)) warning("Both station_id and definition_id are given. Defaulting to station_id.")
  # # Retrieve the most recent definition data for the specified country and station
  # if (!is.null(station_id)){
  #   definitions_data <- get_definitions_data(country = country, station_id = station_id)
  # } else {
  #   definitions_data <- get_definitions_data(country = country, definitions_id = definition_id)
  # }
  # 
  # # If start-of-rains data is not provided, compute it using daily rainfall data
  # if (is.null(start_rains_data)) {
  #   start_rains_data <- update_rainfall_summaries_from_definition(
  #     country = country, 
  #     station_id = station_id, 
  #     daily_data = daily_data, 
  #     summaries = "start_rains"
  #   )
  # }
  # 
  # # Extract the column names for the start-of-rains data
  # data_names <- data_definitions(names(start_rains_data), FALSE, FALSE)
  # 
  # # Retrieve the specified days for calculating season start probabilities
  # start_dates <- as.numeric(definitions_data$season_start_probabilities$specified_day)
  # if (length(start_dates) == 0) {
  #   warning("No specified days given for season start probability. No updates required.")
  # }
  # 
  # # Calculate season start probabilities
  # summary_data <- rpicsa::probability_season_start(
  #   data = start_rains_data,
  #   station = data_names$station,
  #   start_rains = "start_rains_doy",
  #   doy_format = "doy_366", # Assuming day-of-year format is precomputed in start_rains summaries
  #   specified_day = as.integer(start_dates)
  # )
  # 
  return(summary_data)
}

update_crop_success_probabilities_from_definition <- function(country, station_id = NULL, definitions_id = NULL, daily_data, summary_rains_data = NULL) {
  if (!is.null(station_id) & !is.null(definitions_id)) warning("Both station_id and definitions_id are given. Defaulting to station_id.")
  # Retrieve the most recent definition data for the specified country and station
  if (!is.null(station_id)){
    definitions_data <- get_definitions_data(country = country, station_id = station_id)
  } else {
    definitions_data <- get_definitions_data(country = country, definitions_id = definitions_id)
  }
  
  # If start-of-rains data is not provided, compute it using daily rainfall data
  if (is.null(summary_rains_data)) {
    summaries <- "start_rains"
    if (!is.null(definitions_data$end_rains$start_day)){
      summaries <- c(summaries, "end_rains")
      end_doy <- "end_rains_doy"
    }
    if (!is.null(definitions_data$end_season$start_day)){
      summaries <- c(summaries, "end_season")
      end_doy <- "end_season_doy"
    }   
    
    summary_rains_data <- update_rainfall_summaries_from_definition(
      country = country, 
      station_id = station_id, 
      daily_data = daily_data, 
      summaries = summaries
    )
    
    # for end_rains - do we do end_rains by default then otherwise do end_season?
    # what if we have both end reains and end season?
  }
  
  # Extract the column names for the start-of-rains data
  data_names <- data_definitions(names(daily_data), FALSE, FALSE)

  # Retrieve the specified days for calculating season start probabilities
  water_requirements <- as.integer(definitions_data$crops_success$water_requirements)
  planting_dates <- as.integer(definitions_data$crops_success$planting_dates)
  planting_length <- as.integer(definitions_data$crops_success$planting_length)
  summary_data <- NULL
  if (length(water_requirements) == 0){
    warning("No specified days given for water requirements. No updates required.")
    return(summary_data)
  }
  if (length(planting_dates) == 0){
    warning("No specified days given for planting dates No updates required.")
    return(summary_data)
  }
  if (length(planting_length) == 0){
    warning("No specified days given for planting length No updates required.")
    return(summary_data)
  }
  
  start_before_season <- definitions_data$crops_success$start_check
  if (is.null(start_before_season)) start_before_season <- FALSE
  
  # Calculate season start probabilities
  daily_data[[data_names$year]] <- as.factor(as.character(daily_data[[data_names$year]]))
  summary_rains_data[[data_names$year]] <- as.factor(as.character(summary_rains_data[[data_names$year]]))
  summary_data <- rpicsa::crops_definitions(data = daily_data,
                                            date_time  = data_names$date,
                                            station = data_names$station,
                                            year = data_names$year,
                                            rain = data_names$rain,
                                            water_requirements = water_requirements,
                                            planting_dates = planting_dates,
                                            planting_length = planting_length,
                                            start_check = start_before_season,
                                            season_data = summary_rains_data,
                                            start_day = "start_rains_doy",
                                            end_day = end_doy)
  
  return(summary_data)
}

#' Join Null Data
#'
#' This function joins two data frames, `summary_data` and `calculated_data`, 
#' using a full join if `summary_data` is not NULL. If `summary_data` is NULL,
#' it assigns `calculated_data` to `summary_data`.
#'
#' @param summary_data A data frame representing summary data.
#' @param calculated_data A data frame containing calculated data.
#'
#' @return A data frame resulting from the full join of `summary_data` and `calculated_data`,
#' or `calculated_data` if `summary_data` is NULL.
#'
#' @export
#'
#' @examples
#' # summary_data is NULL
#' summary_data <- NULL
#' calculated_data <- data.frame(x = 1:5, y = letters[1:5])
#' join_null_data(summary_data, calculated_data)
#'
#' # summary_data is not NULL
#' summary_data <- data.frame(x = 1:3, y = letters[1:3])
#' calculated_data <- data.frame(x = 4:5, y = letters[4:5])
#' join_null_data(summary_data, calculated_data)
join_null_data <- function(summary_data, calculated_data){
  if (is.null(summary_data)){
    summary_data <- calculated_data
  } else{
    summary_data <- dplyr::full_join(summary_data, calculated_data)
  }
  return(summary_data)
}