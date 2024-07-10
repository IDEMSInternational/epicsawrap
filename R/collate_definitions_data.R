#' Collate Definitions Data for Climatic Analysis from R-Instat
#'
#' This function aggregates various climatic data definitions, including annual summaries, 
#' temperature summaries, crop data, and probabilities of season starts. It is designed to work
#' within a specific context that involves climatic data processing and analysis, particularly
#' focusing on data related to Ghana's climate. The function uses multiple sources of data
#' and calculations to generate a comprehensive list-formatted summary.
#'
#' @param data The name of the main data set, default is "ghana".
#' @param data_by_year The name of the data set that contains data aggregated by year, default is "ghana_by_station_year".
#' @param data_by_year_month The name of the data set that contains data aggregated by year and month, default is NULL.
#' @param crop_data The name of the crop data set, default is "crop_def".
#' @param rain The name of the column containing rainfall data.
#' @param year The name of the column containing year data.
#' @param month The name of the column containing month data.
#' @param summaries The name of the summaries to show. Options are `"annual_rainfall"`, `"annual_temperature"`, `"monthly_temperature"`, `"extremes"`, `"crop_success"`, `"start_season"`.
#' @param start_rains_column The name of the start of rains column in the data.
#' @param start_rains_status_column The name of the start of rains status column in the data.
#' @param end_rains_column The name of the end of rains column in the data.
#' @param end_rains_status_column The name of the end of rains status column in the data.
#' @param end_season_column The name of the end of season column in the data.
#' @param end_season_status_column The name of the end of seasons status column in the data.
#' @param seasonal_length_column The name of the seasonal length column in the data.
#' @param min_tmin_column The name of the minimum of minimum temperature column in the data.
#' @param max_tmin_column The name of the maximum of minimum temperature column in the data.
#' @param mean_tmin_column The name of the mean of minimum temperature column in the data.
#' @param min_tmax_column The name of the minimum of maximum temperature column in the data.
#' @param max_tmax_column The name of the maximum of maximum temperature column in the data.
#' @param mean_tmax_column The name of the mean of maximum temperature column in the data.
#' 
#'@export
#' @return A list that contains the aggregated data definitions.
#' @examples
#' #data_book <- list(get_climatic_column_name = function(data_name, col_name) { return(col_name) },
#' #                  get_calculations = function(data_name) { list() },
#' #                  get_data_frame_metadata = function(data_name) { list() })
#' #collate_definitions_data(data_book = data_book)
#' 
collate_definitions_data <- function(data = "ghana",
                                     data_by_year = "ghana_by_station_year",
                                     data_by_year_month = NULL,
                                     crop_data = "crop_def",
                                     rain = data_book$get_climatic_column_name(data_name = "ghana", "rain"),
                                     year = data_book$get_climatic_column_name("ghana", "year"),
                                     month = data_book$get_climatic_column_name("ghana", "month"),
                                     summaries = c("annual_rainfall", "annual_temperature", "monthly_temperature", "extremes", "crop_success", "start_season"),
                                     start_rains_column = "start_rain", start_rains_status_column = "start_rain_status",
                                     end_rains_column = "end_rains", end_rains_status_column = "end_rain_status", end_season_column = "end_season", 
                                     end_season_status_column = "end_season_status", seasonal_length_column = "seasonal_length",
                                     min_tmin_column = "min_tmin", mean_tmin_column = "mean_tmin", max_tmin_column = "max_tmin",
                                     min_tmax_column = "min_tmax", mean_tmax_column = "mean_tmax", max_tmax_column = "max_tmax"){
  
  definitions_data <- get_r_instat_definitions(data_book$get_calculations(data))
  definitions_year <- get_r_instat_definitions(data_book$get_calculations(data_by_year))
  definitions_offset <- get_offset_term(data)
  
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
    annual_summaries <- build_annual_summaries_definitions(data_name = data,
                                                           data_by_year = definitions_year,
                                                           data = definitions_data,
                                                           rain_name = rain,
                                                           start_rains_column = start_rains_column,
                                                           start_rains_status_column = start_rains_status_column,
                                                           end_rains_column = end_rains_column,
                                                           end_rains_status_column = end_rains_status_column,
                                                           end_season_column = end_season_column,
                                                           end_season_status_column = end_season_status_column,
                                                           seasonal_length_column = seasonal_length_column)
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
      definitions_crop <- data_book$get_data_frame_metadata(crop_data)
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
      definitions_crop <- data_book$get_data_frame_metadata(crop_data)
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
