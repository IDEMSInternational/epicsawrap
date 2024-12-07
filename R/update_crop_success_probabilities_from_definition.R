update_crop_success_probabilities_from_definition <- function(country, station_id = NULL, definitions_id = NULL, daily_data, summary_rains_data = NULL) {
  if (!is.null(station_id) & !is.null(definition_id)) warning("Both station_id and definition_id are given. Defaulting to station_id.")
  # Retrieve the most recent definition data for the specified country and station
  if (!is.null(station_id)){
    definitions_data <- get_definitions_data(country = country, station_id = station_id)
  } else {
    definitions_data <- get_definitions_data(country = country, definition_id = definition_id)
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