# These can be in their own file, in the databook package.

get_rain_count_variable <- function(daily_data_calcs,
                                    rainfall_column = "extreme_rain",
                                    list_name = NULL) {
  result <- list(sign = NA, threshold = NA)
  
  definitions_in_raw <- get_r_instat_definitions(daily_data_calcs)
  
  if (!is.null(definitions_in_raw) && !is.null(rainfall_column)) {
    definition <- definitions_in_raw[[rainfall_column]]
    definition_rain_day_value <- definition$rain_day[[2]]
    
    # Extract components
    result$sign <- sub(".*\\s([<>]=?|==|!=)\\s.*", "\\1", definition_rain_day_value)
    result$threshold <- sub(".*\\s([<>]=?|==|!=)\\s*(-?\\d*\\.?\\d+)\\s*\\)?", "\\2", definition_rain_day_value)
  }
  
  if (!is.null(list_name)) {
    return(setNames(list(result), list_name))
  } else {
    return(result)
  }
}

# Create a file and call it get_epicsa_definitions.R or something. 

get_start_rains_definition <- function(daily_data, summary_data, calculations_data, start_rain = NULL, start_rain_date = NULL, start_rain_status = NULL){
  # 1. Get the offset term
  definitions_offset <- get_offset_term(daily_data) 
  
  # 2. Get the definitions data
  # TODO: For efficiency, we should have that you call in what you want to get definition for (e.g., start_rain, start_rain_date, start_rain_status)
  definitions_year <- get_r_instat_definitions(calculations_data)
  
  # 3. Get the start of rains definitions
  if (!is.null(start_rain)){
    output_value <- "doy"
    start_of_rains <- create_start_rains_definitions(definitions_year[[start_rain]])
    if (!is.null(start_rain_date)){
      output_value <- c(output_value, "date")
    }
  } else {
    if (!is.null(start_rain_date)){
      output_value <- c("date")
      start_of_rains <- create_start_rains_definitions(definitions_year[[start_rain_date]])
    } else if (!is.null(start_rains_status)){
      output_value <- c("status")
      start_of_rains <- create_start_rains_definitions(definitions_year[[start_rain_status]])
    } else {
      return(list())
    }
  }
  if (!is.null(start_rain_status) && !is.null(summary_data[[start_rain_status]])){
    start_of_rains$include_status <- TRUE
    output_value <- unique(c(output_value, "status"))
  }
  start_of_rains$output <- output_value
  start_of_rains$s_start_doy <- definitions_offset
  return(start_of_rains)
}

get_end_rains_definition <- function(daily_data, summary_data, calculations_data, end_rains = NULL, end_rains_date = NULL, end_rains_status = NULL){
  # 1. Get the offset term
  definitions_offset <- get_offset_term(daily_data) # get_offset_term needs the data_book. So this needs to go into another package. 
  
  # 2. Get the definitions data
  # TODO: For efficiency, we should have that you call in what you want to get definition for (e.g., end_rains, end_rains_date, end_rains_status)
  definitions_year <- get_r_instat_definitions(calculations_data)
  
  # 3. Get the start of rains definitions
  if (!is.null(end_rains)){
    output_value <- "doy"
    end_of_rains <- create_end_rains_definitions(definitions_year[[end_rains]])
    if (!is.null(end_rains_date)){
      output_value <- c(output_value, "date")
    }
  } else {
    if (!is.null(end_rains_date)){
      output_value <- c("date")
      end_of_rains <- create_end_rains_definitions(definitions_year[[end_rains_date]])
    } else if (!is.null(end_rains_status)){
      # check this
      output_value <- c("status")
      end_of_rains <- create_end_rains_definitions(definitions_year[[end_rains_status]])
    } else {
      return(list())
    }
  }
  if (!is.null(end_rains_status) && !is.null(summary_data[[end_rains_status]])){
    end_of_rains$include_status <- TRUE
    output_value <- unique(c(output_value, "status"))
  }
  end_of_rains$output <- output_value
  end_of_rains$s_end_doy <- definitions_offset
  return(end_of_rains)
}


get_end_season_definition <- function(daily_data, summary_data, calculations_data, end_season = NULL, end_season_date = NULL, end_season_status = NULL){
  # 1. Get the offset term
  definitions_offset <- get_offset_term(daily_data) # get_offset_term needs the data_book. So this needs to go into another package. 
  
  # 2. Get the definitions data
  # TODO: For efficiency, we should have that you call in what you want to get definition for (e.g., end_season, end_season_date, end_season_status)
  definitions_year <- get_r_instat_definitions(calculations_data)
  
  # 3. Get the start of rains definitions
  if (!is.null(end_season)){
    output_value <- "doy"
    end_of_rains <- create_end_season_definitions(definitions_year[[end_season]])
    if (!is.null(end_season_date)){
      output_value <- c(output_value, "date")
    }
  } else {
    if (!is.null(end_season_date)){
      output_value <- c("date")
      end_of_rains <- create_end_season_definitions(definitions_year[[end_season_date]])
    } else if (!is.null(end_season_status)){
      output_value <- c("status")
      end_of_rains <- create_end_season_definitions(definitions_year[[end_season_status]])
    } else {
      return(list())
    }
  }
  if (!is.null(end_season_status) && !is.null(summary_data[[end_season_status]])){
    end_of_rains$include_status <- TRUE
    output_value <- unique(c(output_value, "status"))
  }
  end_of_rains$output <- output_value
  end_of_rains$s_end_doy <- definitions_offset
  return(end_of_rains)
}

# TODO: test this
get_seasonal_length_definition <- function(calculations_data, seasonal_length){
  # 1. Get the definitions data
  # TODO: For efficiency, we should have that you call in what you want to get definition for (e.g., end_season, end_season_date, end_season_status)
  definitions_year <- get_r_instat_definitions(calculations_data)
  
  # 2. Get the start of rains definitions
  seasonal_length <- create_season_length_definitions(definitions_year[[seasonal_length]])
  
  return(seasonal_length)
}

# Annual and Seasonal Rain Day Definition =========================================================================
get_rainfall_definition <- function(calculations_data, total_rain = NULL, rain_days = NULL,
                                    daily_data_calculation = NULL, rain_days_variable_from = NULL){
  #### 1. Set up
  definitions_year <- get_r_instat_definitions(calculations_data)
  
  #### 2. TOTAL RAINFALL ########
  # variable names for total rain and rainy days
  total_rain_definition <- definitions_year[[total_rain]]
  rain_days_definition <- definitions_year[[rain_days]]
  if (!is.null(total_rain)){
    total_rain <- "TRUE"
  } else {
    total_rain <- "FALSE"
  }
  
  if (!is.null(rain_days)){
    n_rain <- "TRUE"
  } else {
    n_rain <- "FALSE"
  }
  
  # 3. Getting the threshold definition for a rain day from the daily data.
  if (as.logical(n_rain)){
    n_raindays <- get_rain_count_variable(daily_data_calculation, rain_days_variable_from)$threshold
  }
  
  # 4. adding in the params for seasonal/annual_rain ----------------------------------------
  if (as.logical(total_rain) || as.logical(n_rain)) {
    if (as.logical(total_rain) == FALSE) total_rain_definition <- rain_days_definition # setting this so that na.rm can all run for it as a back up.
    na_rm <- extract_value(total_rain_definition$function_exp, "na.rm = ", FALSE)
    na_n <- extract_value(total_rain_definition$function_exp, "na_max_n = ", TRUE)
    na_n_non <- extract_value(total_rain_definition$function_exp, "na_min_n = ", TRUE)
    na_consec <- extract_value(total_rain_definition$function_exp, "na_consecutive_n = ", TRUE)
    na_prop <- extract_value(total_rain_definition$function_exp, "na_max_prop = ", TRUE)
    
    variables_list = c("total_rain", "n_rain", "na_rm",
                       "na_n", "na_n_non", "na_consec", "na_prop")
    
    # Create an empty list
    data_list <- list()
    
    # Loop through variables and add to the list if defined
    for (variable in variables_list) {
      if (exists(variable) && !is.na(get(variable))) {
        data_list[[variable]] <- get(variable)
      } else {
        data_list[[variable]] <- NA
      }
    }
    data_list[["rain_day"]] <- as.numeric(n_raindays) # for rain day threshold
  } else {
    for (variable in variables_list) {
      data_list[[variable]] <- NA
    }
  }
  
  return(data_list)
}

# Getting extreme rainfall values
get_extreme_rain_counts <- function(daily_data_calculation, extreme_rainfall){
  get_rain_count_variable(daily_data_calculation, extreme_rainfall)
}

# Getting spells
get_longest_spell_definitions <- function(calculations_data, spell_column){
  definitions_year <- get_r_instat_definitions(calculations_data)
  
  # Create an empty list
  data_list <- list()
  
  if (!is.null(spell_column)) spell <- definitions_year[[spell_column]]
  else spell <- NULL
  
  if (!is.null(spell)) {
    spell_calculation <- spell$spell_length$spell_day[[2]]
    
    data_list[["spell_from"]] <- as.numeric(sub(".*>=\\s*([0-9]+\\.?[0-9]*).*", "\\1", spell_calculation))
    data_list[["spell_to"]]   <- as.numeric(sub(".*<=\\s*([0-9]+\\.?[0-9]*).*", "\\1", spell_calculation))
  } else {
    data_list[["spell_from"]] <- NA
    data_list[["spell_to"]]   <- NA
  }
  return(data_list)
}



# Crops
get_crop_definitions <- function(definition_file = NULL){
  variables_list <- c("water_requirements", "planting_dates", "planting_length")
  data_list <- list()
  
  get_seq_values <- function(value) {
    if (length(value) < 3) return(value)
    
    from <- value[1]
    to <- value[length(value)]
    by <- (to - from) / (length(value) - 1)
    
    # Reconstruct the sequence
    reconstructed <- seq(from, to, by)
    
    # Check if reconstructed sequence exactly matches original
    if (all.equal(value, reconstructed) == TRUE) {
      return(list(from = from, to = to, by = by))
    } else {
      return(value)
    }
  }
  
  if (!is.null(definition_file)){
    water_requirements <- get_seq_values(unique(definition_file$rain_total))
    planting_dates <- get_seq_values(unique(definition_file$plant_day))
    planting_length <- get_seq_values(unique(definition_file$plant_length))
  }
  
  # Loop through variables and add to the list if defined
  for (variable in variables_list) {
    if (exists(variable) && !anyNA(get(variable))) {
      data_list[[variable]] <- get(variable)
    } else {
      data_list[[variable]] <- NA
    }
  }
  return(data_list)
}


get_season_start_probabilities <- function(definition_file = NULL){
  # Create an empty list
  data_list <- list()
  
  get_seq_values <- function(value) {
    if (length(value) < 3) return(value)
    
    from <- value[1]
    to <- value[length(value)]
    by <- (to - from) / (length(value) - 1)
    
    # Reconstruct the sequence
    reconstructed <- seq(from, to, by)
    
    # Check if reconstructed sequence exactly matches original
    if (all.equal(value, reconstructed) == TRUE) {
      return(list(from = from, to = to, by = by))
    } else {
      return(value)
    }
  }
  
  if (!is.null(definition_file)){
    specified_day <- get_seq_values(unique(definition_file$plant_day))
    data_list[["specified_day"]] <- specified_day
  } else {
    data_list[["specified_day"]] <- NA
  }
  return(data_list)
}

# TEMPERATURE SUMMARIES ----------------------------------------------------------------
get_temperature_summaries <- function(calculations_data, cols){
  definitions_year <- get_r_instat_definitions(calculations_data)
  variables_list = c("na_rm", "na_n", "na_n_non", "na_consec", "na_prop")
  
  get_temperature_na_details <- function(data, temp_col){
    temp_summary_name_list <- NULL
    # Note, we take the na.rm bits from data_by_year
    temp_summary <- data[[temp_col]]
    if (!is.null(temp_summary)){
      na_rm <- extract_value(temp_summary$function_exp, "na.rm = ", FALSE)
      na_n <- extract_value(temp_summary$function_exp, "na_max_n = ", TRUE)
      na_n_non <- extract_value(temp_summary$function_exp, "na_min_n = ", TRUE)
      na_consec <- extract_value(temp_summary$function_exp, "na_consecutive_n = ", TRUE)
      na_prop <- extract_value(temp_summary$function_exp, "na_max_prop = ", TRUE)
      for (variable in variables_list) {
        if (exists(variable) && all(!is.na(get(variable)))) {
          temp_summary_name_list[[variable]] <- get(variable)
        } else {
          temp_summary_name_list[[variable]] <- NA
        }
      }
    }
    return(temp_summary_name_list)
  }
  
  build_block <- function(cols, data) {
    purrr::map(cols, ~ get_temperature_na_details(data, .x)) |>
      purrr::set_names(cols)
  }
  
  temperature_definitions <- build_block(cols = cols, definitions_year)
  return(temperature_definitions)
}






#' Get end rains definitions
#'
#' Retrieves end rains definitions.
#'
#' @param end_rains The end rains data.
#' @return A list representation of end rains definitions.
#' @examples
#' # Example usage:
#' #create_end_rains_definitions(end_rains)
create_end_rains_definitions <- function(end_rains = NULL){
  # Create an empty list
  data_list <- list()
  variables_list = c("start_day", "end_day",  "output", "min_rainfall", "interval_length")
  if (!is.null(end_rains)) {
    start_day <- extract_value(end_rains$filter_2, " >= ")
    end_day <- extract_value(end_rains$filter_2, " <= ")
    output <- "both"
    min_rainfall <- extract_value(end_rains$filter[[1]], "roll_sum_rain > ")
    if (is.null(end_rains$filter$roll_sum_rain[[2]])){
      stop("No roll_sum_rain value found. Have you put end_rains_column to equal 'end_season'? You want end_season_column to equal 'end_season'.")
    }
    interval_length <- extract_value(end_rains$filter$roll_sum_rain[[2]], "n=")
  }
  # Loop through variables and add to the list if defined
  for (variable in variables_list) {
    if (exists(variable) && !is.na(get(variable))) {
      data_list[[variable]] <- get(variable)
    } else {
      data_list[[variable]] <- NA
    }
  }
  
  return(data_list)
}

#' Get end of season definitions
#'
#' Retrieves end season definitions.
#'
#' @param end_season The end season data.
#' @return A list representation of end season definitions.
#' @examples
#' # Example usage:
#' #create_end_season_definitions(end_season)
create_end_season_definitions <- function(end_season = NULL){
  # Create an empty list
  data_list <- list()
  variables_list <- c("start_day", "end_day", "water_balance_max",
                      "capacity", "evaporation", "evaporation_value")
  
  if (!is.null(end_season))  {
    start_day <- extract_value(end_season$filter_2, " >= ")
    end_day <- extract_value(end_season$filter_2, " <= ")
    output <- "both"
    water_balance_max <- extract_value(end_season$filter[[1]], "wb <= ")
    capacity_value <- end_season$filter$conditions_check$wb$wb_max$rain_max[[2]]
    if (is.null(capacity_value)){
      capacity_value <- end_season$filter$wb$wb_max$rain_max[[2]]
      evaporation_value <- end_season$filter$wb$wb_max[[1]]
    } else {
      evaporation_value <- end_season$filter$conditions_check$wb$wb_max[[1]]
    }
    capacity <- extract_value(capacity_value, "yes=")
    evaporation_value <- extract_value(evaporation_value, "rain_max - ")
    if (is.na(evaporation_value)){
      evaporation_value <- end_season$filter$conditions_check$wb$wb_max[[1]]
      if (is.null(evaporation_value)) evaporation_value <- end_season$filter$wb$wb_max[[1]]
      evaporation_value <- extract_value(evaporation_value, "no=", FALSE)
      evaporation <- "variable"
    } else {
      evaporation <- "value"
    }
  }
  # Loop through variables and add to the list if defined
  for (variable in variables_list) {
    if (exists(variable) && !is.na(get(variable))) {
      data_list[[variable]] <- get(variable)
    } else {
      data_list[[variable]] <- NA
    }
  }
  return(data_list)
}

#' Get start of rains definitions
#'
#' Retrieves start rains definitions.
#'
#' @param start_rains The start rains data.
#' @return A list of start of rains definitions
#'
#' @examples
#' # Example usage:
#' #create_start_rains_definitions(start_rains)
create_start_rains_definitions <- function(start_rains = NULL){
  # Create an empty list
  data_list <- list()
  # Create a list
  variables_list = c("start_day", "end_day", "threshold", "total_rainfall", 
                     "over_days", "amount_rain", "proportion", "prob_rain_day", 
                     "dry_spell", "spell_max_dry_days", "spell_interval", 
                     "dry_period", "max_rain", "period_interval", "period_max_dry_days")
  
  if (!is.null(start_rains)) {
    start_day <- extract_value(start_rains$filter_2, " >= ")
    end_day <- extract_value(start_rains$filter_2, " <= ")
    output <- "both"
    
    # Important! Assuming that threshold is the first argument!
    threshold <- extract_value(start_rains$filter[[1]], " >= ")
    
    # if null, then we didn't run it so set that to be false in definitions file.
    if (is.null(start_rains$filter$roll_sum_rain)){
      total_rainfall <- FALSE  
    } else {
      total_rainfall <- TRUE
      if (is.null(start_rains$filter$wet_spell)){
        amount_rain <- extract_value(start_rains$filter[[1]], "roll_sum_rain > ")
        over_days <- extract_value(start_rains$filter$roll_sum_rain[[2]], "n=")
        proportion <- FALSE
      } else {
        prob_rain_day <- extract_value(start_rains$filter$wet_spell[[1]], "probs=")
        over_days <- extract_value(start_rains$filter$wet_spell$roll_sum_rain[[2]], "n=")
        proportion <- TRUE
      }
    }
    if (is.null(start_rains$filter$roll_n_rain_days)){
      number_rain_days <- FALSE 
    } else {
      number_rain_days <- TRUE
      min_rain_days <- extract_value(start_rains$filter[[1]], "roll_n_rain_days >= ")
      rain_day_interval <- extract_value(start_rains$filter$roll_n_rain_days[[1]], "n=")
    }
    if (is.null(start_rains$filter$roll_max_dry_spell)){
      dry_spell <- FALSE
    } else {
      dry_spell <- TRUE
      spell_max_dry_days <- extract_value(start_rains$filter[[1]], "roll_max_dry_spell <= ")
      spell_interval <- extract_value(start_rains$filter$roll_max_dry_spell[[1]], "n=")
    }
    if (is.null(start_rains$filter$n_dry_period)){
      dry_period <- FALSE
    } else {
      dry_period <- TRUE
      max_rain <- extract_value(start_rains$filter$n_dry_period[[1]], "roll_sum_rain_dry_period <= ")
      period_interval <- extract_value(start_rains$filter$n_dry_period[[1]], "n=")
      period_max_dry_days <- extract_value(start_rains$filter$n_dry_period[[1]],
                                           paste0("n=", period_interval, " - "))
    }
  }
  # Loop through each variable in the list
  for (variable in variables_list) {
    # Check if the variable exists and is not NA
    if (exists(variable) && any(!is.na(get(variable)) | !is.null(get(variable)))) {
      # Retrieve the variable's value
      variable_value <- get(variable)
      
      # Check if the variable's class includes "instat_calculation"
      if ("instat_calculation" %in% class(variable_value)) {
        data_list[[variable]] <- NA
      } else {
        data_list[[variable]] <- variable_value
      }
    } else {
      # Assign NA if the variable does not exist or is NA
      data_list[[variable]] <- NA
    }
  }
  return(data_list)
}

#' Get R-Instat definitions
#'
#' Retrieves R-Instat definitions based on given calculations.
#'
#' @param calculation A list of calculations.
#' 
#' @return A list of R-Instat definitions.
#' 
#' @examples
#' # Example usage:
#' #get_r_instat_definitions()
get_r_instat_definitions <- function(calculation){
  manips <- NULL
  type <- NULL
  if (length(calculation) > 0){
    for (i in 1:length(calculation)){
      calc <- calculation[[i]]
      type[i] <- calc$type
      if (type[i] == "summary"){
        # this will tell us if it is DOY or date (or both)
        # run the function with 
        manips[[i]] <- c(variables = calc$calculated_from,
                         function_exp = calc$function_exp, 
                         get_r_instat_definitions(calculation = calc$manipulations))
        type[i] <- calc$result_name
      } else if (type[i] == "by"){
        manips[[i]] <- calc$calculated_from
        type[i] <- paste0("by_", i)
      } else if (type[i] == "filter"){
        if (length(calc$sub_calculations) > 0){
          manips[[i]] <- c(calc$function_exp, get_r_instat_definitions(calculation = calc$sub_calculations))
          type[i] <- paste0("filter")
        } else {
          manips[[i]] <- calc$function_exp
          type[i] <- paste0("filter_2")
        }
      } else if (type[i] == "calculation"){
        if (length(calc$sub_calculations) > 0){
          manips[[i]] <- c(calc$function_exp, get_r_instat_definitions(calculation = calc$sub_calculations))
          type[i] <- calc$result_name
        } else {
          if (length(calc$calculated_from) > 0){
            manips[[i]] <- c(calc$calculated_from, calc$function_exp)        
          } else {
            manips[[i]] <- calc$function_exp
          }
          type[i] <- calc$result_name
        }
        type[i] <- calc$result_name
      }
    }
    if (!is.null(manips)){
      names(manips) <- type
    }
    return(manips)
  }
}