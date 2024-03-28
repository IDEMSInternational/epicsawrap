#' Export R-Instat Data to Google Cloud Storage Bucket
#'
#' This function exports R-Instat data to a specified Google Cloud Storage bucket.
#' It collates the data and saves it as a JSON file on the local machine, then uploads
#' the file to the specified bucket.
#'
#' @param data The main dataset.
#' @param data_by_year The dataset grouped by year.
#' @param data_by_year_month The dataset grouped by year and month.
#' @param crop_data The crop data for if `summaries = "crop_success"`
#' @param start_season_data for if `summaries = "start_season"`
#' @param rain The rainfall data.
#' @param tmin The minimum temperature data.
#' @param tmax The maximum temperature data.
#' @param year The year data.
#' @param month The month data.
#' @param summaries A character vector specifying the types of summaries to include.
#' @param file_path The path to the directory where the JSON file will be saved locally.
#' @param file_name The name of the JSON file (without the ".json" extension).
#' @param country `character(1)` The country code of the data.
#' @param station_id `character` The id's of the stations to analyse. Either a
#'   single value or a vector.
#' @param include_summary_data Logical indicating whether to include summary data in the export.
#'
#' @return NULL (invisibly)
#'
#' @details This function collates the specified data into a JSON format and saves it to the local machine.
#' Then it uploads the JSON file to the specified Google Cloud Storage bucket.
#'
#' @export
#'
#' @examples
#' # Provide examples here if needed
export_r_instat_to_bucket <- function(data, data_by_year, data_by_year_month = NULL, crop_data = NULL,
                                      start_season_data = NULL,
                                      rain = NULL, tmin = NULL, tmax = NULL, year = NULL, month = NULL,
                                      summaries = c("annual_rainfall", "annual_temperature", "monthly_temperature", "extremes", "crop_success", "start_season"),
                                      file_path, file_name,
                                      station_id, country,
                                      include_summary_data = FALSE){
  
  timestamp <- format(Sys.time(), format = "%Y%m%d%H%M%S") 
  
  definitions_data <- epicsadata::collate_definitions_data(data = data,
                                                           data_by_year = data_by_year,
                                                           data_by_year_month = data_by_year_month,
                                                           crop_data = crop_data,
                                                           rain = rain,
                                                           tmin = tmin,
                                                           tmax = tmax,
                                                           year = year,
                                                           month = month,
                                                           summaries = summaries)
  
  # Save onto computer
  jsonlite::write_json(definitions_data,
                       path = paste0(file_path, file_name, ".json"),
                       auto_unbox = TRUE, pretty = TRUE)
  
  # Read from computer to bucket
  add_definitions_to_bucket(country = country, station_id = station_id,
                            new_definitions = paste0(file_path, file_name),
                            timestamp = timestamp)

  if (include_summary_data){
    # function to read summary data from R-Instat into summaries in buckets
    if ("annual_rainfall" %in% summaries){
      add_summaries_to_bucket(country = country, station_id = station_id, data = data_book$get_data_frame(data_by_year),
                              summary = "annual_rainfall_summaries", timestamp = timestamp)
    }
    if ("annual_temperature" %in% summaries){
      add_summaries_to_bucket(country = country, station_id = station_id, data = data_book$get_data_frame(data_by_year),
                              summary = "annual_temperature_summaries", timestamp = timestamp)
    }
    if ("monthly_temperature" %in% summaries){
      add_summaries_to_bucket(country = country, station_id = station_id, data = data_book$get_data_frame(data_by_year_month),
                              summary = "monthly_temperature_summaries", timestamp = timestamp)
    }
    if ("crop_success" %in% summaries){
      add_summaries_to_bucket(country = country, station_id = station_id, data = data_book$get_data_frame(start_season_data),
                              summary = "crop_success_probabilities", timestamp = timestamp)
    }
    if ("start_season" %in% summaries){
      add_summaries_to_bucket(country = country, station_id = station_id, data = data_book$get_data_frame(start_season_data),
                              summary = "season_start_probabilities", timestamp = timestamp)
    }
  }
  return("Uploaded to Bucket")
}
