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
#' @param tmin The minimum temperature data.
#' @param tmax The maximum temperature data.
#' @param year The year data.
#' @param month The month data.
#' @param summaries A character vector specifying the types of summaries to include.
#' @param country `character(1)` The country code of the data.
#' @param station_id `character` The id's of the stations to analyse. Either a
#'   single value or a vector.
#' @param definition_id `character` The ID to give to the definition file. 
#' @param include_summary_data Logical indicating whether to include summary data in the export.
#' @param annual_rainfall_data Annual rainfall summary data.
#' @param annual_temperature_data Annual temperature summary data.
#' @param monthly_temperature_data Monthly temperature summary data.
#' @param crop_success_data The proportion crop data output. Used if `summaries = "crop_success"` when `include_summary_data = TRUE`. This is called `crop_prop` by default in R-Instat.
#' @param season_start_data The crop data to be read into `"season_start_probabilities"`. This is called `crop_def` by default in R-Instat.
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
                                      rain = NULL, tmin = NULL, tmax = NULL, year = NULL, month = NULL,
                                      summaries = c("annual_rainfall", "annual_temperature", "monthly_temperature", "extremes", "crop_success", "start_season"),
                                      station_id, definition_id, country,
                                      include_summary_data = FALSE,
                                      annual_rainfall_data = NULL, annual_temperature_data = NULL, monthly_temperature_data = NULL,
                                      crop_success_data = NULL, season_start_data = NULL){
  
  timestamp <- format(Sys.time(), format = "%Y%m%d%H%M%S") 

  definitions_data <- epicsadata::collate_definitions_data(data = data, data_by_year = data_by_year, data_by_year_month = data_by_year_month, crop_data = crop_data_name, rain = rain, tmin = tmin, tmax = tmax, year = year, month = month, summaries = summaries)
  # Save into bucket
  purrr::map(.x = station_id,
             .f = ~add_definitions_to_bucket2(country = country, station_id = .x, new_definitions = definitions_data, timestamp = timestamp))

  if (include_summary_data){
    # function to read summary data from R-Instat into summaries in buckets
    if ("annual_rainfall" %in% summaries) purrr::map(.x = station_id, .f = ~add_summaries_to_bucket2(country = country, station_id = .x, data = annual_rainfall_data, summary = "annual_rainfall_summaries", timestamp = timestamp))

    if ("annual_temperature" %in% summaries) purrr::map(.x = station_id, .f = ~add_summaries_to_bucket2(country = country, station_id = .x, data = annual_temperature_data, summary = "annual_temperature_summaries", timestamp = timestamp))

    if ("monthly_temperature" %in% summaries) purrr::map(.x = station_id, .f = ~add_summaries_to_bucket2(country = country, station_id = .x, data = monthly_temperature_data, summary = "monthly_temperature_summaries", timestamp = timestamp))

    if ("crop_success" %in% summaries) purrr::map(.x = station_id, .f = ~add_summaries_to_bucket2(country = country, station_id = .x, data = crop_success_data, summary = "crop_success_probabilities", timestamp = timestamp))

    if ("start_season" %in% summaries) purrr::map(.x = station_id, .f = ~add_summaries_to_bucket2(country = country, station_id = .x, data = season_start_data, summary = "season_start_probabilities", timestamp = timestamp))
  }
  
  update_metadata_definition_id(country = country, station_id = station_id, definition_id = definition_id, overwrite = FALSE)
  
  return("Uploaded to Bucket")
}
