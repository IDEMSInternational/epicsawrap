#' Collate Definitions Data for Climatic Analysis from R-Instat
#'
#' This function aggregates climatic metadata definitions used in R-Instat into a structured list format.
#' It supports annual rainfall, temperature summaries (annual and monthly), crop success proportions, 
#' and seasonal onset probabilities. The resulting definitions can be exported or used in downstream analysis.
#'
#' This function is especially designed for use in sub-Saharan African contexts like Ghana, but is general enough 
#' to be reused in other national datasets following similar structures.
#'
#' @param data_by_year The name of the dataset aggregated by year. This is the main source for rainfall and annual temperature definitions.
#' @param data_by_year_month The name of the dataset aggregated by year and month. Required for monthly temperature summaries.
#' @param crop_data The name of the crop-related data set (e.g., `"crop_def"`), used for crop success and season start definitions.
#' @param rain The name of the rainfall column.
#' @param year The name of the year column.
#' @param month The name of the month column.
#' @param summaries A character vector specifying which summaries to extract. Options include: 
#' `"annual_rainfall"`, `"annual_temperature"`, `"monthly_temperature"`, `"crop_success"`, `"start_season"`.
#' @param start_rains_column Name of the start-of-rains column (e.g., `"start_rains_doy"`).
#' @param start_rains_status_column Name of the column indicating the start-of-rains success status.
#' @param end_rains_column Name of the end-of-rains column.
#' @param end_rains_status_column Name of the column indicating end-of-rains status.
#' @param end_season_column Name of the end-of-season column.
#' @param end_season_status_column Name of the column indicating end-of-season status.
#' @param seasonal_length_column Name of the seasonal length column.
#' @param rain_days_name (Optional) Name of the rain-day threshold variable, if applicable.
#' @param extreme_rainfall_column (Optional) Name of the extreme rainfall threshold variable, if applicable.
#' @param data (Optional) Dataset name used to extract definitions for rain days or extreme rainfall if needed.
#' @param annual_total_rain_col (Optional) Column name for total annual rainfall values.
#' @param seasonal_total_rain_col (Optional) Column name for seasonal total rainfall values.
#' @param annual_rainday_col (Optional) Column name for total annual rain day counts.
#' @param seasonal_rainday_col (Optional) Column name for seasonal rain day counts.
#' @param min_tmin_column Column name for the minimum of daily minimum temperatures.
#' @param mean_tmin_column Column name for the mean of daily minimum temperatures.
#' @param max_tmin_column Column name for the maximum of daily minimum temperatures.
#' @param min_tmax_column Column name for the minimum of daily maximum temperatures.
#' @param mean_tmax_column Column name for the mean of daily maximum temperatures.
#' @param max_tmax_column Column name for the maximum of daily maximum temperatures.
#'
#' @return A named list of definition components used for climate summary calculations. 
#' This may include sections such as `annual_rain`, `start_rains`, `crop_success_probabilities`, etc.,
#' depending on the summaries selected.
#'
#' @details
#' This function calls sub-functions to parse and interpret definitions from R-Instat's metadata system,
#' resolving duplicate definitions and offset terms, and integrating additional logic for rainfall and temperature components.
#'
#' @examples
#' \dontrun{
#' definitions <- collate_definitions_data(
#'   data_by_year = "ghana_by_station_year",
#'   crop_data = "crop_def",
#'   rain = "rain",
#'   year = "year",
#'   month = "month",
#'   summaries = c("annual_rainfall", "crop_success")
#' )
#' }
#'
#' @export
collate_definitions_data <- function(data_by_year = "ghana_by_station_year",
                                     data_by_year_month = NULL,
                                     crop_data = "crop_def",
                                     rain = data_book$get_climatic_column_name(data_name = "ghana", "rain"),
                                     year = data_book$get_climatic_column_name("ghana", "year"),
                                     month = data_book$get_climatic_column_name("ghana", "month"),
                                     summaries = c("annual_rainfall", "annual_temperature", "monthly_temperature", "crop_success", "start_season"),
                                     start_rains_column = "start_rains_doy", start_rains_status_column = "start_rain_status",
                                     end_rains_column = "end_rains_doy", end_rains_status_column = "end_rain_status", end_season_column = "end_season_doy", 
                                     end_season_status_column = "end_season_status", seasonal_length_column = "season_length",
                                     rain_days_name = NULL, extreme_rainfall_column = NULL, data = NULL,
                                     annual_total_rain_col = NULL, seasonal_total_rain_col = NULL,
                                     annual_rainday_col = NULL, seasonal_rainday_col = NULL,
                                     min_tmin_column = "min_tmin", mean_tmin_column = "mean_tmin", max_tmin_column = "max_tmin",
                                     min_tmax_column = "min_tmax", mean_tmax_column = "mean_tmax", max_tmax_column = "max_tmax"){
  

  # get definitions from calculations
  definitions_year <- get_r_instat_definitions(data_book$get_calculations(data_by_year))

  definitions_in_raw <- NULL
  if (!is.null(rain_days_name) || !is.null(extreme_rainfall_column)) {
    if (!is.null(data)) {
      definitions_in_raw <- get_r_instat_definitions(data_book$get_calculations(data))
    }
  }
  
  definitions_offset <- get_offset_term(data_by_year)
  
  if (length(names(definitions_year)) != length(unique(names(definitions_year)))){
    # Identify duplicates
    duplicates <- names(definitions_year)[duplicated(names(definitions_year)) | duplicated(names(definitions_year), fromLast = TRUE)]
    # Get unique duplicates
    unique_duplicates <- unique(duplicates)
    warning(paste0("Some elements are repeated: (", unique_duplicates, "). Taking the most recent version."))
    definitions_year <- definitions_year[!duplicated(definitions_year, fromLast = TRUE)]
  }
  
  # if yes to annual summaries - give the data frame "ghana_by_station_year"
  if ("annual_rainfall" %in% summaries){
    annual_summaries <- build_annual_summaries_definitions(data_by_year = definitions_year,
                                                           rain_name = rain,
                                                           start_rains_column = start_rains_column,
                                                           start_rains_status_column = start_rains_status_column,
                                                           end_rains_column = end_rains_column,
                                                           end_rains_status_column = end_rains_status_column,
                                                           end_season_column = end_season_column,
                                                           end_season_status_column = end_season_status_column,
                                                           seasonal_length_column = seasonal_length_column,
                                                           definitions_in_raw = definitions_in_raw,
                                                           rain_days_name = rain_days_name,
                                                           extreme_rainfall_column = extreme_rainfall_column,
                                                           annual_total_rain_col = annual_total_rain_col,
                                                           seasonal_total_rain_col = seasonal_total_rain_col,
                                                           annual_rainday_col = annual_rainday_col,
                                                           seasonal_rainday_col = seasonal_rainday_col)
    if(!is.null(definitions_offset) || definitions_offset != 1){
      annual_summaries$start_rains$s_start_doy <- definitions_offset
      annual_summaries$end_rains$s_start_doy <- definitions_offset
      annual_summaries$end_season$s_start_doy <- definitions_offset
      annual_summaries$annual_rain$s_start_doy <- definitions_offset
    }
  } else {
    annual_summaries <- NULL
  }
  
  if (any(grepl("_temperature", summaries))){
    # if yes to annual temperature summaries - give the data frame "ghana_by_station_year"
    # if yes to monthly temperature summaries - give the data frame "ghana_by_station_year_month"
    if ("annual_temperature" %in% summaries){
      annual_temp <- definitions_year
    } else {
      annual_temp <- NULL
    }
    if ("monthly_temperature" %in% summaries){
      if (!is.null(data_by_year_month)){
        definitions_year_month <- get_r_instat_definitions(data_book$get_calculations(data_by_year_month))
      } else {
        stop("monthly temperature requested but no data_by_year_month file given.")
      }
    } else {
      definitions_year_month <- NULL
    }
  } else {
    annual_temp <- NULL
    definitions_year_month <- NULL
  }
  temperature_summaries <- build_total_temperature_summaries(data_by_year = annual_temp,
                                                             data_by_year_month = definitions_year_month,
                                                             year = year,
                                                             month = month,
                                                             min_tmin_column = min_tmin_column, 
                                                             mean_tmin_column = mean_tmin_column, 
                                                             max_tmin_column = max_tmin_column,
                                                             min_tmax_column = min_tmax_column, 
                                                             mean_tmax_column = mean_tmax_column, 
                                                             max_tmax_column = max_tmax_column)
  if((!is.null(definitions_offset) || definitions_offset != 1) & (any(grepl("_temperature", summaries)))){
    temperature_summaries$min_tmin$s_start_doy <- definitions_offset
    temperature_summaries$max_tmin$s_start_doy <- definitions_offset
    temperature_summaries$min_tmax$s_start_doy <- definitions_offset
    temperature_summaries$max_tmax$s_start_doy <- definitions_offset
    temperature_summaries$mean_tmin$s_start_doy <- definitions_offset
    temperature_summaries$mean_tmax$s_start_doy <- definitions_offset
  }
  
  # if yes to crop success then ...
  if ("crop_success" %in% summaries){
    if (!is.null(crop_data)){
      definitions_crop <- data_book$get_data_frame(crop_data)
    } else {
      stop("Crop summaries requested but no crop_data file given.")
    }
  } else {
    definitions_crop <- NULL
  }
  crop_summaries <- build_crop_definitions(definitions_crop)

  # if yes to probabilities
  if ("start_season" %in% summaries){
    if (!is.null(crop_data)){
      definitions_crop <- data_book$get_data_frame(crop_data)
    } else {
      stop("Season start summaries requested but no crop_data file given.")
    }
  } else {
    definitions_crop <- NULL
  }
  season_start_summaries <- build_season_start_probabilities(definitions_crop)
  
  # extremes then ...
  
  # overall:
  data_list <- c(annual_summaries, temperature_summaries, crop_summaries, season_start_summaries)
  
  # remove anything of length 0 
  # Define a function to check the length of each element
  length_not_zero <- function(element) {
    length(element) > 0
  }
  
  # Filter the list x to remove elements with length 0
  data_list <- Filter(length_not_zero, data_list)
  
  return(data_list)
}
