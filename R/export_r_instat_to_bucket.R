#' Export R-Instat Data and Definitions to Google Cloud Storage Bucket
#'
#' This function collates R-Instat climatic definitions and (optionally) summary data,
#' saves the result as a JSON file, and uploads it to a specified Google Cloud Storage bucket.
#' It supports multiple summary types such as rainfall, temperature, crop success, and season start probabilities.
#'
#' @param data The original data frame (Optional. Only required for definitions in calculations for number of rainy days and extremes. Default `NULL`).
#' @param data_by_year The name of the dataset grouped by year. Required for definitions and rainfall/temperature summaries.
#' @param data_by_year_month The name of the dataset grouped by year and month (optional). Required for monthly temperature summaries.
#' @param crop_data_name The name of the crop data used for crop success and season start probabilities.
#' @param rain The name of the rainfall column.
#' @param station The name of the station column used to split and export data.
#' @param year The name of the year column.
#' @param month The name of the month column.
#' @param summaries A character vector of summaries to include. Options: `"annual_rainfall"`, `"annual_temperature"`, `"monthly_temperature"`, `"crop_success"`, `"start_season"`.
#' @param station_id Character vector of station IDs to process.
#' @param definitions_id A string identifying the definition version (used for filename and metadata tracking).
#' @param country The ISO country code (e.g., `"GH"` for Ghana).
#' @param include_summary_data Logical; if `TRUE`, also uploads summary data in addition to definitions.
#' @param annual_rainfall_data Data frame of annual rainfall summary data.
#' @param annual_temperature_data Data frame of annual temperature summary data.
#' @param monthly_temperature_data Data frame of monthly temperature summary data.
#' @param crop_success_data Data frame of crop success proportions (usually called `crop_prop` in R-Instat).
#' @param season_start_data Data frame used for season start probabilities (usually `crop_def`).
#' @param start_rains_column Column name for start of rains (DOY).
#' @param start_rains_status_column Column name indicating success status for start of rains.
#' @param end_rains_column Column name for end of rains (DOY).
#' @param end_rains_status_column Column name indicating success status for end of rains.
#' @param end_season_column Column name for end of season (DOY).
#' @param end_season_status_column Column name indicating success status for end of season.
#' @param seasonal_length_column Column name indicating seasonal length (in days).
#' @param longest_rain_spell_col Column name indicating the longest spell (for rainfall in days).
#' @param longest_tmin_spell_col Column name indicating the longest spell (for tmin in days).
#' @param longest_tmax_spell_col Column name indicating the longest spell (for tmax in days).
#' @param rain_days_name Column name used to define a rain day threshold (optional).
#' @param extreme_rainfall_column Column name used to define an extreme rainfall threshold (optional).
#' @param extreme_tmin_column Column name used to define an extreme tmin threshold (optional).
#' @param extreme_tmax_column Column name used to define an extreme tmax threshold (optional).
#' @param annual_total_rain_col Column name used for annual total rainfall values (optional).
#' @param seasonal_total_rain_col Column name for seasonal total rainfall values (optional).
#' @param annual_rainday_col Column name for annual rain day count (optional).
#' @param seasonal_rainday_col Column name for seasonal rain day count (optional).
#' @param min_tmin_column Column name for minimum of minimum temperatures.
#' @param max_tmin_column Column name for maximum of minimum temperatures.
#' @param mean_tmin_column Column name for mean of minimum temperatures.
#' @param min_tmax_column Column name for minimum of maximum temperatures.
#' @param max_tmax_column Column name for maximum of maximum temperatures.
#' @param mean_tmax_column Column name for mean of maximum temperatures.
#'
#' @return A string message confirming that the definitions and summaries (if included) were uploaded to the cloud storage bucket.
#'
#' @details
#' This function:
#' \enumerate{
#'   \item Collates all relevant climatic definition metadata using `collate_definitions_data()`.
#'   \item Uploads definitions to the bucket using `add_definitions_to_bucket()`.
#'   \item Updates metadata to record the new definition ID for each station.
#'   \item (Optionally) uploads associated summary datasets per station (if `include_summary_data = TRUE`).
#' }
#' The upload timestamp is included in all files to ensure version tracking. Each summary is saved per station.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' export_r_instat_to_bucket(
#'   data_by_year = "ghana_by_station_year",
#'   crop_data_name = "crop_def",
#'   rain = "rain", station = "station", year = "year", month = "month",
#'   summaries = c("annual_rainfall", "crop_success"),
#'   country = "GH", definitions_id = "def_202506", station_id = c("ST01", "ST02"),
#'   include_summary_data = TRUE,
#'   annual_rainfall_data = ghana_rainfall_summaries,
#'   crop_success_data = crop_prop,
#'   season_start_data = crop_def
#' )
#' }
export_r_instat_to_bucket <- function(data = NULL, data_by_year, data_by_year_month = NULL, crop_data_name = NULL,
                                      rain = NULL, station = NULL, year = NULL, month = NULL,
                                      summaries = c("annual_rainfall", "annual_temperature", "monthly_temperature", "crop_success", "start_season"),
                                      station_id = NULL, definitions_id, country,
                                      include_summary_data = FALSE,
                                      annual_rainfall_data = NULL, annual_temperature_data = NULL, monthly_temperature_data = NULL,
                                      crop_success_data = NULL, season_start_data = NULL,
                                      
                                      # from annual_rainfall data
                                      start_rains_column = "start_rains_doy", start_rains_status_column = "start_rain_status",
                                      end_rains_column = "end_rains_doy", end_rains_status_column = "end_rains_status", end_season_column = "end_season_doy",
                                      end_season_status_column = "end_season_status", seasonal_length_column = "season_length",
                                      longest_rain_spell_col = "long_spell_rains", longest_tmin_spell_col = "long_spell_tmin", longest_tmax_spell_col = "long_spell_tmax",
                                      
                                      # from main data frame
                                      rain_days_name = NULL, extreme_rainfall_column = NULL, extreme_tmin_column = NULL, extreme_tmax_column = NULL, 
                                      annual_total_rain_col = NULL, seasonal_total_rain_col = NULL,
                                      annual_rainday_col = NULL, seasonal_rainday_col = NULL,
                                      
                                      # for temperature
                                      min_tmin_column = "min_tmin", mean_tmin_column = "mean_tmin", max_tmin_column = "max_tmin",
                                      min_tmax_column = "min_tmax", mean_tmax_column = "mean_tmax", max_tmax_column = "max_tmax"){
  
  timestamp <- format(Sys.time(), format = "%Y%m%d%H%M%S") 
  definitions_data <- collate_definitions_data(data_by_year = data_by_year, data_by_year_month = data_by_year_month, crop_data = crop_data_name, rain = rain, year = year, month = month, summaries = summaries,
                                               start_rains_column = start_rains_column, start_rains_status_column = start_rains_status_column, end_rains_column = end_rains_column,
                                               end_rains_status_column = end_rains_status_column, 
                                               end_season_column = end_season_column,
                                               end_season_status_column = end_season_status_column, 
                                               seasonal_length_column = seasonal_length_column, 
                                               longest_rain_spell_col = longest_rain_spell_col, 
                                               longest_tmin_spell_col = longest_tmin_spell_col, 
                                               longest_tmax_spell_col = longest_tmax_spell_col,
                                               rain_days_name = rain_days_name, extreme_rainfall_column = extreme_rainfall_column, 
                                               extreme_tmin_column = extreme_tmin_column, extreme_tmax_column = extreme_tmax_column, data = data,
                                               annual_total_rain_col = annual_total_rain_col,
                                               seasonal_total_rain_col = seasonal_total_rain_col,
                                               annual_rainday_col = annual_rainday_col,
                                               seasonal_rainday_col = seasonal_rainday_col,
                                               min_tmin_column = min_tmin_column, mean_tmin_column = mean_tmin_column, max_tmin_column = max_tmin_column,
                                               min_tmax_column = min_tmax_column, mean_tmax_column = mean_tmax_column, max_tmax_column = max_tmax_column)

  # Save into bucket
  # commented out code was when we had this for multiple station_ids. We now just do for one definition_id.
  #purrr::map(.x = station_id,
  #           .f = ~add_definitions_to_bucket(country = country, station_id = .x, new_definitions = definitions_data, timestamp = timestamp))
  add_definitions_to_bucket(country = country, definitions_id = definitions_id, new_definitions = definitions_data, timestamp = timestamp)
  
  # Ensure unique stations are obtained because we want to repeat for each station
  unique_stations <- unique(data_book$get_data_frame(data_by_year)[[station]])
  
  # TODO: need to add in metadata additions
  purrr::map(.x = unique_stations,
             .f = ~{station_id <- .x
             update_metadata_definition_id(country = country, station_id = .x, definition_id = definitions_id, overwrite = FALSE)
             })
  
  if (include_summary_data){
    # function to read summary data from R-Instat into summaries in buckets
    # different file per station:
    if ("annual_rainfall" %in% summaries) {
      # this is for all our rainfall stuff in our "_by_year" data frame, essentially. Except for temperature summaries.
      purrr::map(.x = unique_stations,
        .f = ~{station_id <- .x
          filtered_data <- annual_rainfall_data %>% filter(station == station_id)
          add_summaries_to_bucket(country = country, station_id = station_id, data = filtered_data,
            summary = "annual_rainfall_summaries", timestamp = timestamp)
        }
      )
    }
    
    if ("annual_temperature" %in% summaries) {
      purrr::map(
        .x = unique_stations,
        .f = ~{station_id <- .x
          filtered_data <- annual_temperature_data %>% filter(station == station_id)
          add_summaries_to_bucket(country = country, station_id = station_id, data = filtered_data,
            summary = "annual_temperature_summaries", timestamp = timestamp)
        }
      )
    }
    
    if ("monthly_temperature" %in% summaries) {
      purrr::map(
        .x = unique_stations,
        .f = ~{station_id <- .x
        filtered_data <- monthly_temperature_data %>% filter(station == station_id)
        add_summaries_to_bucket(country = country, station_id = station_id, data = filtered_data,
                                summary = "monthly_temperature_summaries", timestamp = timestamp)
        }
      )
    }
    if ("crop_success" %in% summaries) {
      purrr::map(
        .x = unique_stations,
        .f = ~{station_id <- .x
        filtered_data <- crop_success_data %>% filter(station == station_id)
        add_summaries_to_bucket(country = country, station_id = station_id, data = filtered_data,
                                summary = "crop_success_probabilities", timestamp = timestamp)
        }
      )
    }
    if ("start_season" %in% summaries) {
      purrr::map(
        .x = unique_stations,
        .f = ~{station_id <- .x
        filtered_data <- season_start_data %>% filter(station == station_id)
        add_summaries_to_bucket(country = country, station_id = station_id, data = filtered_data,
                                summary = "season_start_probabilities", timestamp = timestamp)
        }
      )
    }
  }
  
  return("Uploaded to Bucket")
}