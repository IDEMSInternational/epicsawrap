#' Export R-Instat Data to Google Cloud Storage Bucket
#'
#' This function exports R-Instat data to a specified Google Cloud Storage bucket.
#' It collates the data and saves it as a JSON file on the local machine, then uploads
#' the file to the specified bucket.
#'
#' @param data The main dataset.
#' @param data_by_year The dataset grouped by year.
#' @param data_by_year_month The dataset grouped by year and month.
#' @param crop_data_name Name of the crop data used for definitions when `summaries = "crop_success"`.
#' @param rain The rainfall data.
#' @param year The year data.
#' @param month The month data.
#' @param summaries A character vector specifying the types of summaries to include.
#' @param country `character(1)` The country code of the data.
#' @param station_id `character` The id's of the stations to analyse. Either a
#'   single value or a vector.
#' @param definitions_id `character` The ID to give to the definition file. 
#' @param include_summary_data Logical indicating whether to include summary data in the export.
#' @param annual_rainfall_data Annual rainfall summary data.
#' @param annual_temperature_data Annual temperature summary data.
#' @param monthly_temperature_data Monthly temperature summary data.
#' @param crop_success_data The proportion crop data output. Used if `summaries = "crop_success"` when `include_summary_data = TRUE`. This is called `crop_prop` by default in R-Instat.
#' @param season_start_data The crop data to be read into `"season_start_probabilities"`. This is called `crop_def` by default in R-Instat.
#' @param start_rains_column The name of the start of rains column in the data.
#' @param start_rains_status_column The name of the start of rains status column in the data.
#' @param end_rains_column The name of the end of rains column in the data.
#' @param end_season_column The name of the end of season column in the data.
#' @param seasonal_length_column The name of the seasonal length column in the data.
#' @param min_tmin_column The name of the minimum of minimum temperature column in the data.
#' @param max_tmin_column The name of the maximum of minimum temperature column in the data.
#' @param mean_tmin_column The name of the mean of minimum temperature column in the data.
#' @param min_tmax_column The name of the minimum of maximum temperature column in the data.
#' @param max_tmax_column The name of the maximum of maximum temperature column in the data.
#' @param mean_tmax_column The name of the mean of maximum temperature column in the data.
#'
#' @return A message confirming that the data has been uploaded to the bucket.
#'
#' @details This function collates the specified data into a JSON format and saves it to the local machine.
#' Then it uploads the JSON file to the specified Google Cloud Storage bucket. If \code{include_summary_data} is TRUE,
#' it also uploads additional summary data to the bucket.
#'
#' @export
#'
#' @examples
#' # Provide examples here if needed
export_r_instat_to_bucket <- function(data, data_by_year, data_by_year_month = NULL, crop_data_name = NULL,
                                      rain = NULL, year = NULL, month = NULL,
                                      summaries = c("annual_rainfall", "annual_temperature", "monthly_temperature", "extremes", "crop_success", "start_season"),
                                      station_id, definitions_id, country,
                                      include_summary_data = FALSE,
                                      annual_rainfall_data = NULL, annual_temperature_data = NULL, monthly_temperature_data = NULL,
                                      crop_success_data = NULL, season_start_data = NULL,
                                      start_rains_column = "start_rain", start_rains_status_column = "start_rain_status",
                                      end_rains_column = "end_rains", end_season_column = "end_season", seasonal_length_column = "seasonal_length",
                                      min_tmin_column = "min_tmin", mean_tmin_column = "mean_tmin", max_tmin_column = "max_tmin",
                                      min_tmax_column = "min_tmax", mean_tmax_column = "mean_tmax", max_tmax_column = "max_tmax"){
  
  
  timestamp <- format(Sys.time(), format = "%Y%m%d%H%M%S") 
  
  definitions_data <- epicsadata::collate_definitions_data(data = data, data_by_year = data_by_year, data_by_year_month = data_by_year_month, crop_data = crop_data_name, rain = rain, year = year, month = month, summaries = summaries,
                                                           start_rains_column = start_rains_column, start_rains_status_column = start_rains_status_column, end_rains_column = end_rains_column, end_season_column = end_season_column, seasonal_length_column = seasonal_length_column, min_tmin_column = min_tmin_column, 
                                                           mean_tmin_column = mean_tmin_column, max_tmin_column = max_tmin_column, min_tmax_column = min_tmax_column, mean_tmax_column = mean_tmax_column, max_tmax_column = max_tmax_column )
  
  # Save into bucket
  # commented out code was when we had this for multiple station_ids. We now just do for one definition_id.
  #purrr::map(.x = station_id,
  #           .f = ~add_definitions_to_bucket(country = country, station_id = .x, new_definitions = definitions_data, timestamp = timestamp))
  
  add_definitions_to_bucket(country = country, definitions_id = definitions_id, new_definitions = definitions_data, timestamp = timestamp)
  update_metadata_definition_id(country = country, station_id = station_id, definition_id = definitions_id, overwrite = FALSE)
  
  if (include_summary_data){
    # function to read summary data from R-Instat into summaries in buckets
    if ("annual_rainfall" %in% summaries) purrr::map(.x = station_id, .f = ~add_summaries_to_bucket(country = country, station_id = .x, data = annual_rainfall_data, summary = "annual_rainfall_summaries", timestamp = timestamp))
    
    if ("annual_temperature" %in% summaries) purrr::map(.x = station_id, .f = ~add_summaries_to_bucket(country = country, station_id = .x, data = annual_temperature_data, summary = "annual_temperature_summaries", timestamp = timestamp))
    
    if ("monthly_temperature" %in% summaries) purrr::map(.x = station_id, .f = ~add_summaries_to_bucket(country = country, station_id = .x, data = monthly_temperature_data, summary = "monthly_temperature_summaries", timestamp = timestamp))
    
    if ("crop_success" %in% summaries) purrr::map(.x = station_id, .f = ~add_summaries_to_bucket(country = country, station_id = .x, data = crop_success_data, summary = "crop_success_probabilities", timestamp = timestamp))
    
    if ("start_season" %in% summaries) purrr::map(.x = station_id, .f = ~add_summaries_to_bucket(country = country, station_id = .x, data = season_start_data, summary = "season_start_probabilities", timestamp = timestamp))
  }
  
  return("Uploaded to Bucket")
}