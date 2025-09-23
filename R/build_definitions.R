#' Build Annual Summaries Definitions for Climatic Analysis
#'
#' Constructs a structured list of annual summary definitions for use in climate-related analyses.
#' These summaries typically include: start of rains, end of rains, end of season, seasonal length,
#' total rainfall, count of rain days, and number of extreme rain days. It supports both standard
#' and derived indicators, with optional metadata drawn from calculations in the raw dataset.
#'
#' @param data_name Name of the dataset being referenced. (Retained for compatibility; not currently used directly.)
#' @param data_by_year A named list of summary definitions from the yearly-aggregated dataset, typically from `get_r_instat_definitions()`.
#' @param definitions_in_raw (Optional) Raw definition metadata, typically obtained using `get_r_instat_definitions()` on the unaggregated dataset. Required if `extreme_rainfall_column` is used.
#' @param rain_days_name Name of the indicator used to define rain days (e.g., `"count"`). Default is `"count"`.
#' @param extreme_rain_name Name of the indicator used to define extreme rain days. Default is `"extreme_rain"`.
#' @param annual_total_rain_col Column name for total annual rainfall (optional).
#' @param seasonal_total_rain_col Column name for total seasonal rainfall (optional).
#' @param annual_rainday_col Column name for count of annual rain days (optional).
#' @param seasonal_rainday_col Column name for count of seasonal rain days (optional).
#' @param start_rains_column Column name representing the start of rains (DOY).
#' @param start_rains_status_column Column indicating status for start of rains.
#' @param end_rains_column Column name representing the end of rains (DOY).
#' @param end_rains_status_column Column indicating status for end of rains.
#' @param end_season_column Column name representing the end of the season (DOY).
#' @param end_season_status_column Column indicating status for end of season.
#' @param seasonal_length_column Column name representing seasonal length (in days).
#' @param longest_rain_spell_col Column name indicating the longest spell (for rainfall in days).
#' @param longest_tmin_spell_col Column name indicating the longest spell (for tmin in days).
#' @param longest_tmax_spell_col Column name indicating the longest spell (for tmax in days).
#' @param extreme_rainfall_column (Optional) Name of the column used in the raw definitions to define extreme rainfall threshold (e.g., `"(rainfall >= 40)"`).
#' @param extreme_tmin_column (Optional) Name of the column used in the raw definitions to define extreme tmin threshold (e.g., `"(tmin <= 15)"`).
#' @param extreme_tmax_column (Optional) Name of the column used in the raw definitions to define extreme tmax threshold (e.g., `"(tmax >= 30)"`).
#'
#' @return A named list of annual summaries including:
#' \describe{
#'   \item{start_rains}{Definition for start of rains, with optional status logic.}
#'   \item{end_rains}{Definition for end of rains.}
#'   \item{end_season}{Definition for end of season.}
#'   \item{season_length}{Definition for seasonal length.}
#'   \item{annual_rain}{Definition for total rainfall and rain days.}
#'   \item{extreme_rain}{Definition for extreme rain day count, if available.}
#' }
#'
#' @details
#' This function merges multiple sub-definitions, combining both time-based and rainfall-based
#' summaries into one unified structure. If rain day or extreme rainfall definitions are specified,
#' it pulls metadata from `definitions_in_raw` to determine appropriate thresholds or logic.
#'
#' @examples
#' \dontrun{
#' defs <- build_annual_summaries_definitions(
#'   data_name = "ghana",
#'   data_by_year = get_r_instat_definitions("ghana_by_station_year"),
#'   start_rains_column = "start_rains_doy",
#'   start_rains_status_column = "start_rain_status",
#'   end_rains_column = "end_rains_doy",
#'   end_rains_status_column = "end_rain_status",
#'   end_season_column = "end_season_doy",
#'   end_season_status_column = "end_season_status",
#'   seasonal_length_column = "season_length",
#'   extreme_rainfall_column = "extreme_rain",
#'   extreme_tmin_column = "extreme_tmin",
#'   extreme_tmax_column = "extreme_tmax"
#' )
#' }
#'
build_annual_summaries_definitions <- function(data_name, data_by_year,
                                               definitions_in_raw = NULL,
                                               rain_days_name = "count", # the name of the indicator for rainy days (e.g., count) created for the n_rain_days, etc from our main data frame
                                               extreme_rain_name = "extreme_rain", # the name of indicator for if it was an extreme rainy day or not, created for the n_rain_days, etc from our main data frame
                                               annual_total_rain_col = NULL, seasonal_total_rain_col = NULL,
                                               annual_rainday_col = NULL, seasonal_rainday_col = NULL,
                                               start_rains_column = NULL, start_rains_status_column = NULL,
                                               end_rains_column = NULL, end_rains_status_column = NULL, end_season_column = NULL,
                                               end_season_status_column = NULL, seasonal_length_column = NULL,
                                               longest_rain_spell_col = NULL, longest_tmin_spell_col = NULL, longest_tmax_spell_col = NULL,
                                               extreme_rainfall_column = NULL, extreme_tmin_column = NULL, extreme_tmax_column = NULL){
  start_of_rains <- NULL
  end_rains <- NULL
  end_season <- NULL
  seasonal_length <- NULL
  total_rain_counts <- NULL
  extreme_rain_counts <- NULL
  extreme_tmin_counts <- NULL
  extreme_tmax_counts <- NULL
  if (!is.null(start_rains_column)) start_of_rains <- get_start_rains_definitions(data_by_year[[start_rains_column]])
  if (!is.null(end_rains_column)) end_rains <- get_end_rains_definitions(data_by_year[[end_rains_column]])
  if (!is.null(end_season_column)) end_season <- get_end_season_definitions(data_by_year[[end_season_column]])
  if (!is.null(seasonal_length_column)) seasonal_length <- get_season_length_definitions(data_by_year[[seasonal_length_column]])

  if (!is.null(start_rains_status_column) && !is.null(data_by_year[[start_rains_status_column]])) start_of_rains$start_rains$include_status <- TRUE
  if (!is.null(end_rains_status_column) && !is.null(data_by_year[[end_rains_status_column]])) end_rains$end_rains$include_status <- TRUE
  if (!is.null(end_season_status_column) && !is.null(data_by_year[[end_season_status_column]])) end_season$end_season$include_status <- TRUE
  
  # for annual rainfall / rainy days in year:
  total_rain_counts <- get_total_rain_counts(data_by_year,
                                             annual_total_rain_col = annual_total_rain_col,
                                             seasonal_total_rain_col = seasonal_total_rain_col,
                                             annual_rainday_col = annual_rainday_col,
                                             seasonal_rainday_col = seasonal_rainday_col,
                                             definitions_in_raw = definitions_in_raw,
                                             rain_days_name = rain_days_name)
  
  # For getting spells definitions
  longest_rain_spell <- get_longest_spell_definitions(data_by_year, longest_rain_spell_col, "longest_rain_spell")
  longest_tmin_spell <- get_longest_spell_definitions(data_by_year, longest_tmin_spell_col, "longest_tmin_spell")
  longest_tmax_spell <- get_longest_spell_definitions(data_by_year, longest_tmax_spell_col, "longest_tmax_spell")

  # We want this to work for extreme_rain_name, but we need access to the raw data for this.
  # We don't need the raw data. Just access to it so we can get the calculations. Perhaps then this is optional to them if they want to 
  # give it or not. 
  # we can get the raw data frame if they supply the rain_days_name and extreme_rain_name (rain days), but, I don't know if this is OK
  # We only need the raw data for the summaries in it!
  # if they are protective over their raw data. (we do not need the data itself, just the summaries within it)
  extreme_rain_counts <- get_extreme_rain_counts(definitions_in_raw, extreme_rainfall_column)
  extreme_tmin_counts <- get_extreme_rain_counts(definitions_in_raw, extreme_tmin_column, "extreme_tmin")
  extreme_tmax_counts <- get_extreme_rain_counts(definitions_in_raw, extreme_tmax_column, "extreme_tmax")
  
  # Get the list of summaries:
  summaries_list <- c(start_of_rains, end_rains, end_season, seasonal_length, total_rain_counts,
                      extreme_rain_counts, extreme_tmin_counts, extreme_tmax_counts,
                      longest_rain_spell, longest_tmin_spell, longest_tmax_spell)
  return(summaries_list)
}

#' Build Crop Definitions from File
#'
#' This function reads crop definition data from a provided file structure generated in R-Instat.
#' It then extracts information about water requirements, planting dates, and planting length for different crops.
#' The extracted values are then split into lists..
#'
#' @param definition_file A list containing file data and attributes generated in R-Instat with
#' named vectors `Var1`, `Var2`, and `Var3` for water requirements, planting dates, and planting length respectively.
#'
#' @return A list representing the structured crop definition data, including water requirements, 
#' planting dates, and planting length.
#'
#' @examples
#' # Assuming definition_file is a correctly structured list:
#' #get_crop_definitions(definition_file)
build_crop_definitions <- function(definition_file = NULL){
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
      data_list[["crops_success"]][[variable]] <- get(variable)
    } else {
        data_list[["crops_success"]][[variable]] <- NA
    }
  }
  return(data_list)
}
#' Build Season Start Probabilities from File
#'
#' This function processes a file structure to extract information about the specified day for season start probabilities.
#' The information is split into lists
#'
#' @param definition_file A list containing file data and attributes generated in R-Instat.
#'
#' @return A list representing the season start probabilities with the specified days.
#'
#' @examples
#' #get_season_start_probabilities(definition_file)
build_season_start_probabilities <- function(definition_file = NULL){
  
  # Create an empty list
  data_list <- list()
  data_list[["season_start_probabilities"]] <- list()
  
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
    data_list[["season_start_probabilities"]][["specified_day"]] <- specified_day
  } else {
    data_list[["season_start_probabilities"]][["specified_day"]] <- NA
  }
  return(data_list)
}

#' Build Spells Definitions from File
#'
#' This function reads spell data from a provided file structure generated in R-Instat.
#' It then extracts information about the parameter values given to construct these spells.
#'
#' @param spells_data Data frame used for spells data (usually `spells`).
#' 
#' @return A list representing the structured crop definition data, including water requirements, 
#' planting dates, and planting length.
#'
#' @examples
#' # Assuming definition_file is a correctly structured list:
#' #get_crop_definitions(definition_file)
build_spells_summaries <- function(spells_data = NULL){
  if (is.null(spells_data)) { spells_definitions <- NULL}
  else spells_definitions <- get_r_instat_definitions(data_book$get_calculations(spells_data))
  
  # Create an empty list
  data_list <- list()
  data_list[["spells"]] <- list()
  # Create a list
  variables_list = c("start_day", "end_day", "sign", "from", "to")
  
  # TODO: not working for start/end day in R-Instat at the moment.
  spells_calculation <- spells_definitions$filter$spell[[2]]
  if (!is.null(spells_calculation)){  
    from <- extract_value(spells_calculation, " >= ")
    to <- extract_value(spells_calculation, " <= ")
    
    if (!is.na(from) && !is.na(to)){
      sign_check <- grepl("&", spells_calculation)
      if (sign_check) sign <- "between"
      else (sign <- "unknown")
    } else if (is.na(from) && !is.na(to)){
      sign <- "equal to or less than"
    } else if (!is.na(from) && is.na(to)){
      sign <- "equal to or greater than"
    } else if (is.na(from) & is.na(to)){
      from <- extract_value(spells_calculation, " > ")
      to <- extract_value(spells_calculation, " < ")
      sign <- grepl("|", spells_calculation)
      if (sign) sign <- "excluding between"
      else (sign <- "unknown")
    } else {
      from <- NA
      to <- NA
      sign <- "unknown"
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
        data_list[["spells"]][[variable]] <- NA
      } else {
        data_list[["spells"]][[variable]] <- variable_value
      }
    } else {
      # Assign NA if the variable does not exist or is NA
      data_list[["spells"]][[variable]] <- NA
    }
  }
  return(data_list)
}
#' Calculate total temperature summaries
#'
#' Calculates total temperature summaries based on provided parameters.
#'
#' @param data_by_year A list of temperature summaries by definition (e.g., year).
#' @param data_by_year_month An optional second list of temperature summaries by definition (e.g., year and month).
#' @param min_tmin_column The name of the minimum of minimum temperature column in the data.
#' @param max_tmin_column The name of the maximum of minimum temperature column in the data.
#' @param mean_tmin_column The name of the mean of minimum temperature column in the data.
#' @param min_tmax_column The name of the minimum of maximum temperature column in the data.
#' @param max_tmax_column The name of the maximum of maximum temperature column in the data.
#' @param mean_tmax_column The name of the mean of maximum temperature column in the data.
#' @param min_monthly_tmin_column Column name for minimum of minimum temperatures (for monthly temperature data).
#' @param max_monthly_tmin_column Column name for maximum of minimum temperatures (for monthly temperature data).
#' @param mean_monthly_tmin_column Column name for mean of minimum temperatures (for monthly temperature data).
#' @param min_monthly_tmax_column Column name for minimum of maximum temperatures (for monthly temperature data).
#' @param max_monthly_tmax_column Column name for maximum of maximum temperatures (for monthly temperature data).
#' @param mean_monthly_tmax_column Column name for mean of maximum temperatures (for monthly temperature data).
#' @return A list containing total temperature summaries.
#' 
#' @examples
#' # Example usage:
build_total_temperature_summaries <- function(data_by_year,
                                              data_by_year_month = NULL,
                                              min_tmin_column = NULL,
                                              mean_tmin_column = NULL,
                                              max_tmin_column = NULL,
                                              min_tmax_column = NULL,
                                              mean_tmax_column = NULL,
                                              max_tmax_column = NULL,
                                              min_monthly_tmin_column = NULL,
                                              mean_monthly_tmin_column = NULL,
                                              max_monthly_tmin_column = NULL,
                                              min_monthly_tmax_column = NULL,
                                              mean_monthly_tmax_column = NULL,
                                              max_monthly_tmax_column = NULL){
  build_block <- function(min_col, mean_col, max_col, var, data, to) {
    cols <- c(min = min_col, mean = mean_col, max = max_col)
    purrr::map(cols, ~ get_temp_summaries(.x, data, to = to)) |>
      purrr::set_names(paste0(names(cols), "_", var))
  }
  
  annual <- c(
    build_block(min_tmin_column, mean_tmin_column, max_tmin_column,
                "tmin", data_by_year, "annual"),
    build_block(min_tmax_column, mean_tmax_column, max_tmax_column,
                "tmax", data_by_year, "annual")
  )
  
  monthly <- c(
    build_block(min_monthly_tmin_column, mean_monthly_tmin_column, max_monthly_tmin_column,
                "tmin", data_by_year_month, "monthly"),
    build_block(min_monthly_tmax_column, mean_monthly_tmax_column, max_monthly_tmax_column,
                "tmax", data_by_year_month, "monthly")
  )
  
  data_list <-  list(annual = annual, monthly = monthly)
  
  names(data_list) <- c("annual_temperature_summaries",
                        "monthly_temperature_summaries")
  
  return(data_list)
}
