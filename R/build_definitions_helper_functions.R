#' Get end rains definitions
#'
#' Retrieves end rains definitions.
#'
#' @param end_rains The end rains data.
#' @return A list representation of end rains definitions.
#' @examples
#' # Example usage:
#' #get_end_rains_definitions(end_rains)
get_end_rains_definitions <- function(end_rains = NULL){
  # Create an empty list
  data_list <- list()
  data_list[["end_rains"]] <- list()
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
      data_list[["end_rains"]][[variable]] <- get(variable)
    } else {
      data_list[["end_rains"]][[variable]] <- NA
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
#' #get_end_season_definitions(end_season)
get_end_season_definitions <- function(end_season = NULL){
  # Create an empty list
  data_list <- list()
  data_list[["end_season"]] <- list()
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
      data_list[["end_season"]][[variable]] <- get(variable)
    } else {
      data_list[["end_season"]][[variable]] <- NA
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
#' #get_start_rains_definitions(start_rains)
get_start_rains_definitions <- function(start_rains = NULL){
  # Create an empty list
  data_list <- list()
  data_list[["start_rains"]] <- list()
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
    if (exists(variable) && !is.na(get(variable))) {
      # Retrieve the variable's value
      variable_value <- get(variable)
      
      # Check if the variable's class includes "instat_calculation"
      if ("instat_calculation" %in% class(variable_value)) {
        data_list[["start_rains"]][[variable]] <- NA
      } else {
        data_list[["start_rains"]][[variable]] <- variable_value
      }
    } else {
      # Assign NA if the variable does not exist or is NA
      data_list[["start_rains"]][[variable]] <- NA
    }
  }
  return(data_list)
}

#' Get season length definitions
#'
#' Retrieves season length definitions.
#'
#' @param length The season length data.
#' @return A list representation of season length definitions.
#' @examples
#' # Example usage:
#' #get_season_length_definitions(length)
get_season_length_definitions <- function(length = NULL){ # TODO: it should be called "season" not "end_season"
  # Create an empty list
  data_list <- list()
  variables_list <- c("end_type")
  
  if (!is.null(length)) {
    end_type <- sub(" - .*", "", length[[3]])
    end_type <- sub(".*?_", "", end_type)
  }
  # Loop through variables and add to the list if defined
  for (variable in variables_list) {
    if (exists(variable) && !is.na(get(variable))) {
      data_list[["seasonal_length"]][[variable]] <- get(variable)
    } else {
      data_list[["seasonal_length"]][[variable]] <- NA
    }
  }
  return(data_list)
}

#' Extract Extreme Rainfall Threshold Definition
#'
#' This is a convenience wrapper around [get_rain_counts()] specifically for extracting
#' the threshold and comparison operator used to define an "extreme rain day" condition,
#' such as `"(rainfall >= 40)"`. The result is returned as a list nested under the
#' `"extreme_rain"` key.
#'
#' @param definitions_in_raw A named list of raw definitions, typically obtained from
#'   `get_r_instat_definitions()` on the unaggregated dataset.
#' @param extreme_rainfall_column The name of the rainfall definition to extract from.
#'   Default is `"extreme_rain"`.
#' @param list_name The name to give the new item in the list.
#'
#' @return A list with one element named `"extreme_rain"`, containing:
#' \describe{
#'   \item{sign}{The extracted comparison operator (e.g., `>=`, `<`).}
#'   \item{threshold}{The threshold value as a character string (e.g., `"40"`).}
#' }
#' If the definition is missing or malformed, both elements are returned as `NA`.
#'
#' @examples
#' #defs <- list(extreme_rain = list(rain_day = list(NULL, "(rainfall >= 40)")))
#'
#' # Returns: list(extreme_rain = list(sign = ">=", threshold = "40"))
#' #get_extreme_rain_counts(defs)
get_extreme_rain_counts <- function(definitions_in_raw = NULL,
                                    extreme_rainfall_column = "extreme_rain",
                                    list_name = "extreme_rain") {
  get_rain_counts(definitions_in_raw, extreme_rainfall_column, list_name = list_name)
}

#' Get Longest Spell Definitions
#'
#' Extract the numeric lower and upper bounds from a spell‑length calculation
#' expression stored in a year’s definitions list.
#'
#' @param definitions_year A list (or similar structure) containing spell definitions
#'   for a specific year. One element should hold an object with a
#'   `spell_length$spell_day` entry whose second element is a string like
#'   `"(rain >= X) & rain <= Y"`.
#' @param spell_column A string or integer specifying which element of
#'   `definitions_year` contains the spell information. If `NULL`, no spell is
#'   extracted and both bounds return as `NA`.
#' @param list_name Character. The name to use for the returned sub‑list. Defaults
#'   to `"longest_spell"`.
#'
#' @return A named list of length 1. The single element is named by `list_name`
#'   and contains a list with two numeric components:
#'   - `spell_from`: the lower bound \(X\) extracted from the expression, or `NA`.
#'   - `spell_to`: the upper bound \(Y\) extracted from the expression, or `NA`.
#'
#' @details
#' This function looks up `definitions_year[[spell_column]]`, then expects to find
#' a string at
#' `spell$spell_length$spell_day[[2]]` matching the pattern
#' `"...>= X ...<= Y"`. It uses regular expressions to pull out the numbers
#' `X` (after `>=`) and `Y` (after `<=`). If the column is missing or `NULL`,
#' both `spell_from` and `spell_to` are set to `NA`.
#'
#' @examples
#' definitions_year <- list(
#'   my_spell = list(
#'     spell_length = list(
#'       spell_day = list(NULL, "(rain >= 0.85) & rain <= 2.3")
#'     )
#'   )
#' )
#' get_longest_spell_definitions(definitions_year, "my_spell")
#' # $longest_spell
#' # $longest_spell$spell_from
#' # [1] 0.85
#' #
#' # $longest_spell$spell_to
#' # [1] 2.3
get_longest_spell_definitions <- function(definitions_year, spell_column, list_name = "longest_spell"){
  # Create an empty list
  data_list <- list()
  
  if (!is.null(spell_column)) spell <- definitions_year[[spell_column]]
  else spell <- NULL
  
  if (!is.null(spell)) {
    spell_calculation <- spell$spell_length$spell_day[[2]]

    data_list[[list_name]][["spell_from"]] <- as.numeric(sub(".*>=\\s*([0-9]+\\.?[0-9]*).*", "\\1", spell_calculation))
    data_list[[list_name]][["spell_to"]]   <- as.numeric(sub(".*<=\\s*([0-9]+\\.?[0-9]*).*", "\\1", spell_calculation))
  } else {
    data_list[[list_name]][["spell_from"]] <- NA
    data_list[[list_name]][["spell_to"]]   <- NA
  }
  
  return(data_list)
}

#' Extract Rainfall Threshold and Comparison Operator
#'
#' This function parses a rainfall condition string of the form `"(rainfall >= 40)"`
#' to extract both the comparison operator (e.g., `>=`) and the threshold value (e.g., `40`).
#' It is used to identify how rain days or extreme rainfall conditions are defined
#' in the metadata generated by R-Instat.
#'
#' @param definitions_in_raw A named list containing raw metadata definitions, typically
#'   generated by `get_r_instat_definitions()` on the unaggregated data.
#' @param rainfall_column The name of the variable in `definitions_in_raw` that contains the
#'   rain condition (e.g., `"extreme_rain"` or `"count"`).
#' @param list_name Optional. If provided, the result will be nested in a named list with this key
#'   (e.g., `list(extreme_rain = list(...))`). If `NULL` (default), returns a flat list with
#'   elements `sign` and `threshold`.
#'
#' @return A list containing:
#' \describe{
#'   \item{sign}{The extracted comparison operator (e.g., `>=`, `<`, `==`).}
#'   \item{threshold}{The extracted numeric threshold as a character string (e.g., `"40"` or `"-12.5"`).}
#' }
#' If `list_name` is supplied, the result is nested inside a named list.
#' If the required structure or value is missing, both elements default to `NA`.
#'
#' @examples
#' defs <- list(extreme_rain = list(rain_day = list(NULL, "(rainfall >= 40)")))
#'
#' # Returns: list(sign = ">=", threshold = "40")
#' get_rain_counts(defs, "extreme_rain")
#'
#' # Returns: list(extreme_rain = list(sign = ">=", threshold = "40"))
#' get_rain_counts(defs, "extreme_rain", "extreme_rain")
get_rain_counts <- function(definitions_in_raw = NULL,
                            rainfall_column = "extreme_rain",
                            list_name = NULL) {
  result <- list(sign = NA, threshold = NA)
  
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

#' Get temperature summaries
#'
#' Retrieves temperature summaries based on provided parameters.
#'
#' @param temp_summary_name Character vector specifying the name of the temperature summary.
#' @param data A list of temperature summaries by definition (e.g., year or month).
#' @param to A vector stating if it is annual or monthly summaries.
#' @return A list containing temperature summary information.
get_temp_summaries <- function(temp_summary_name, data, to = c("annual", "monthly")){
  
  temp_summary_name_list <- NULL
  variables_list = c("to", "na_rm", "na_n", "na_n_non", "na_consec", "na_prop")
  
  # if there is neither, return the definitions in an empty file
  if (is.null(data)){
    for (variable in variables_list) {
      temp_summary_name_list[[variable]] <- NA
    }
  } else {
    # Note, we take the na.rm bits from data_by_year
    temp_summary <- data[[temp_summary_name]]
    temp_summary_2 <- data[[temp_summary_name]]
    if (!is.null(temp_summary)){
      to <- to
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
    } else {
      temp_summary <- temp_summary_2
      temp_summary$by_1 <- temp_summary$by_2
      for (variable in variables_list) {
        temp_summary_name_list[[variable]] <- NA
      }
    } 
  }
  return(temp_summary_name_list)
}

#' Extract Annual and Seasonal Rainfall Definitions
#'
#' This function retrieves metadata definitions for total rainfall and the number of rainy days, 
#' separately for annual and seasonal periods. It searches the supplied summary definitions for 
#' rainfall-related indicators, using naming conventions and filter criteria to distinguish 
#' annual from seasonal data. Optionally, it also extracts the rain-day threshold (e.g., `>= 1 mm`) 
#' from raw metadata.
#' 
#' @param data_by_year A named list of summary definitions, typically returned by 
#'   [get_r_instat_definitions()], for the annual dataset.
#' @param annual_total_rain_col (Optional) Column name for total annual rainfall.
#' @param seasonal_total_rain_col (Optional) Column name for total seasonal rainfall.
#' @param annual_rainday_col (Optional) Column name for the annual rain-day count.
#' @param seasonal_rainday_col (Optional) Column name for the seasonal rain-day count.
#' @param definitions_in_raw (Optional) Raw metadata definitions (from unaggregated data), 
#'   used to extract the rain-day threshold.
#' @param rain_days_name (Optional) Name of the indicator in the raw metadata (e.g., `"count"`) 
#'   used to define a rain-day threshold.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{annual_rain}{List of summary parameters for annual rainfall totals and rain-day counts.}
#'   \item{seasonal_rain}{List of summary parameters for seasonal rainfall totals and rain-day counts.}
#' }
#' Each of these lists may include:
#' \itemize{
#'   \item `total_rain` – indicator for total rainfall.
#'   \item `n_rain` – indicator for number of rain days.
#'   \item `rain_day` – rain-day threshold definition.
#'   \item `na_rm`, `na_n`, `na_n_non`, `na_consec`, `na_prop` – metadata about missing-value handling.
#' }
#' If a definition cannot be found or is ambiguous, the corresponding values will be set to `NA`.
#'
#' @details
#' The function attempts to match rainfall indicators using the column names provided 
#' (e.g., `"sum_rain"`, `"count_rain"`).  
#' Heuristics are then applied to decide if a match refers to annual or seasonal data:
#' \itemize{
#'   \item Definitions filtered by variables like `start_rain` and `end_` are considered seasonal.
#'   \item Definitions without such filters are assumed to be annual.
#' }
#' If `definitions_in_raw` and `rain_days_name` are provided, the function calls 
#' [get_rain_counts()] on the raw metadata to retrieve the rain-day threshold.
get_total_rain_counts <- function(data_by_year = NULL,
                                  annual_total_rain_col = NULL, seasonal_total_rain_col = NULL,
                                  annual_rainday_col = NULL, seasonal_rainday_col = NULL,
                                  definitions_in_raw = NULL, rain_days_name = NULL){
  
  # issue in getting seasonal_rain -- because we run the check for seasonal rain:
  # need to read in that start_rain column name, for one, 
  # but also is this even the right code?
  # grepl("start_rain", sum_rain_i$filter_2) && grepl("end_", sum_rain_i$filter_2)
  
  variables_list = c("total_rain", "n_rain", "rain_day", "na_rm",
                     "na_n", "na_n_non", "na_consec", "na_prop")
  annual_rain_definition <- NULL
  seasonal_rain_definition <- NULL
  
  # set as FALSE, to be set as TRUE if they're present
  annual_total_rain <- "FALSE"
  annual_count_raindays <- "FALSE"
  annual_n_raindays <- NULL # always NULL?
  
  seasonal_total_rain <- "FALSE"
  seasonal_count_raindays <- "FALSE"
  seasonal_n_raindays <- NULL # always NULL?
  
  # if we aren't calling in data_by_year, then set everything as NA
  if (is.null(data_by_year)){
    for (variable in variables_list) {
      annual_rain_definition[["annual_rain"]][[variable]] <- NA
    }
    for (variable in variables_list) {
      seasonal_rain_definition[["seasonal_rain"]][[variable]] <- NA
    }
  } else {
    # get seasonal_sum_rain as the definition even if there are multiple.
    # and annual_sum_rain even if multiple
    # This is found by seeing which are
    # called sum_<rainfallname>, and then checking if there is filter_by_year for seasonal
    data_by_year_names <- names(data_by_year)
    
    #### TOTAL RAINFALL ########################################################
    # which of data_by_year are "sum_<rainfall>" -----------------------------
    # these are going to be potentially annual_rain$total_rain
    # and seasonal_rain$total_rain
    # and $na_rm $na_n $na_n_non $na_consec $na_prop
    total_rain_cols <- c(annual_total_rain_col, seasonal_total_rain_col)
    
    for (rain_col in total_rain_cols){
      count_names <- grepl(rain_col, data_by_year_names)
      
      # so for sum_rainfall columns - the cols counting the rainfall:
      if (sum(count_names) > 0){
        sum_rain_lists <- which(count_names == TRUE)
        for (i in sum_rain_lists){
          sum_rain_i <- data_by_year[[i]]
          
          # check if there is filtering - if it is filtered based on start and end rains then it is seasonal.
          # TODO: this isn't true. fix this. 
          if (any(grepl("filter", names(sum_rain_i)))){
            if (grepl("start_rain", sum_rain_i$filter_2) && grepl("end_", sum_rain_i$filter_2)){
              seasonal_total_rain <- "TRUE"
              seasonal_sum_rain <- sum_rain_i
            } else if (grepl(">= 1", sum_rain_i$filter_2) && grepl("<= 36", sum_rain_i$filter_2)){
              annual_total_rain <- "TRUE"
              annual_sum_rain <- sum_rain_i
            }
            # if there is no filtering, then it is annual.
          } else {
            annual_total_rain <- "TRUE"
            annual_sum_rain <- sum_rain_i
          }
        }
      }
    }
    
    #### N RAINY DAYS ##########################################################
    # read in the name of the rainday count column for annual (and for seasonal)
    total_rainday_cols <- c(annual_rainday_col, seasonal_rainday_col)
    # these are going to be potentially annual_rain$n_rain
    # and seasonal_rain$n_rain
    
    # loop through those total_rainday_cols to find any
    for (rain_col in total_rainday_cols){
      count_names <- grepl(rain_col, data_by_year_names)
      
      # so for sum_rainday columns - the cols counting the rainfall:
      if (sum(count_names) > 0){
        sum_rain_lists <- which(count_names == TRUE)
        for (i in sum_rain_lists){
          sum_rain_i <- data_by_year[[i]]
          
          # check if there is filtering - if it is filtered based on start and end rains then it is seasonal.
          # TODO: this isn't true. fix this. 
          if (any(grepl("filter", names(sum_rain_i)))){
            if (grepl("start_rain", sum_rain_i$filter_2) && grepl("end_", sum_rain_i$filter_2)){
              seasonal_count_raindays <- "TRUE"
              seasonal_sum_rain <- sum_rain_i
              # <=36 to encapsulate 365, 366
            } else if (grepl(">= 1", sum_rain_i$filter_2) && grepl("<= 36", sum_rain_i$filter_2)){
              annual_count_raindays <- "TRUE"
              annual_sum_rain <- sum_rain_i
            }
            # if there is no filtering, then it is annual.
          } else {
            annual_count_raindays <- "TRUE"
            annual_sum_rain <- sum_rain_i
          }
        }
      }
    }
    
    #### N RAINY DAYS ##########################################################
    # Then the rainy day threshold value (n_rain)
    # if we want to know the threshold for rainy days, then look in the raw data definitions (not the raw data)
    # rain_days_name = the name indicating if it is a rainy day or not in the raw data.
    # only runs if rain_days_name and definitions_in_raw (i.e., raw data) is given.
    annual_n_raindays <- get_rain_counts(definitions_in_raw, rain_days_name)$threshold
    
    # TODO: set up for seasonal
    ############################################################################
    
    
    # adding in the $ params for annual_rain ----------------------------------------
    if (any(as.logical(annual_total_rain)) || any(as.logical(annual_count_raindays))) {
      annual_rain_definition <- sum_rain_definitions(time = "annual_rain",
                                                     total_rain = annual_total_rain,
                                                     n_rain = annual_count_raindays,
                                                     sum_rain = annual_sum_rain,
                                                     n_raindays = annual_n_raindays,
                                                     data = NULL)
    } else {
      for (variable in variables_list) {
        annual_rain_definition[["annual_rain"]][[variable]] <- NA
      }
    }
    
    # adding in the $ params for seasonal_rain
    if (any(as.logical(seasonal_total_rain)) || any(as.logical(seasonal_count_raindays))) {
      seasonal_rain_definition <- sum_rain_definitions(time = "seasonal_rain",
                                                       total_rain = seasonal_total_rain,
                                                       n_rain = seasonal_count_raindays,
                                                       sum_rain = seasonal_sum_rain,
                                                       n_raindays = seasonal_n_raindays)#, data = data_definition)
    } else {
      for (variable in variables_list) {
        seasonal_rain_definition[["seasonal_rain"]][[variable]] <- NA
      }
    }
    
    # # todo: how to ensure we're getting the correct count.
    # if (annual_count_raindays){
    #   data_definition <- get_r_instat_definitions(data_book$get_calculations(data_name))
    # } else {
    #   if (seasonal_count_raindays){
    #     data_definition <- get_r_instat_definitions(data_book$get_calculations(data_name))
    #   } else {
    #     data_definition <- NULL
    #   }
    # }
  }
  
  rain_definitions <- c(annual_rain_definition, seasonal_rain_definition)
  return(rain_definitions)
}

#' Extract Value
#'
#' Extracts a specific value from a string using a regular expression.
#'
#' @param string The input string.
#' @param value_expr The regular expression pattern to extract the value.
#' @param as_numeric Logical indicating whether the extracted value should be converted to numeric.
#' @return The extracted value.
#' @export
#' 
#' @examples
#' # Example usage:
#' extract_value("Example string with value: 123.45", "\\d+(\\.\\d+)?")
extract_value <- function(string, value_expr, as_numeric = TRUE){
  if (as_numeric){
    value <- stringr::str_match(string, paste0(value_expr, "([0-9]+(?:\\.[0-9]+)?)"))[1, 2]
    value <- as.numeric(value)
  } else {
    value <- gsub("\\)", "", stringr::str_match(string, paste0(value_expr, "([^\\s,]+)")))[1, 2]
  }
  return(value)
}

#' Summarize Rain Definitions
#'
#' This function summarises rain definitions for a specific time period.
#'
#' @param time A character string specifying the time period for which the rain definitions are summarised (`"annual_rain"` or `"seasonal_rain""`).
#' @param total_rain Logical indicating whether total rain is considered.
#' @param n_rain Logical indicating whether the number of rainy days is considered.
#' @param sum_rain Numeric vector containing the sum of rainfall.
#' @param n_raindays Numeric vector containing the number of rainy days.
#' @param data Optional additional data (default `NULL`).
#' 
#' @return A list containing summarised rain definitions for the specified time period.
sum_rain_definitions <- function(time = "annual_rain", total_rain,
         n_rain, sum_rain,
         n_raindays, data = NULL){
  data_list <- list()
  data_list[[time]] <- list()
  # if (n_rain){
  #   rain_day <- extract_value(data$count$rain_day[[2]], " >= ", FALSE)
  # }
  sum_rain <- c(sum_rain)
  
  na_rm <- extract_value(sum_rain$function_exp, "na.rm = ", FALSE)
  na_n <- extract_value(sum_rain$function_exp, "na_max_n = ", TRUE)
  na_n_non <- extract_value(sum_rain$function_exp, "na_min_n = ", TRUE)
  na_consec <- extract_value(sum_rain$function_exp, "na_consecutive_n = ", TRUE)
  na_prop <- extract_value(sum_rain$function_exp, "na_max_prop = ", TRUE)
  
  variables_list = c("total_rain", "n_rain", "na_rm",
                     "na_n", "na_n_non", "na_consec", "na_prop")
  
  # Create an empty list
  data_list <- list()
  data_list[[time]] <- list()
  
  # Loop through variables and add to the list if defined
  for (variable in variables_list) {
    if (exists(variable) && !is.na(get(variable))) {
      data_list[[time]][[variable]] <- get(variable)
    } else {
      data_list[[time]][[variable]] <- NA
    }
  }
  data_list[[time]][["rain_day"]] <- as.numeric(n_raindays)
  return(data_list)
}
