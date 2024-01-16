#' Add New Definitions to Google Cloud Storage Bucket
#'
#' This function adds a new definitions file with a timestamp to a specified Google Cloud Storage (GCS) bucket. The new definitions are sourced from a JSON file provided as input.
#'
#' @param country A character vector specifying the country or countries from which to get the definitions data. Options are `"mz"` and `"zm"`.
#' @param station_id A character string specifying the ID of the station for which to get the definitions data.
#' @param new_definitions A character vector specifying the path to the JSON file containing the new definitions.
#'
#' @export
#' @details
#' The function creates a timestamp in the format "YYYYMMDDHHMMSS" and appends it to the station_id to form the filename. It then reads the provided JSON file, creates a new JSON file with the timestamped filename, and uploads it to the specified GCS bucket.
add_definitions_to_bucket <- function(country, station_id, new_definitions){
  return(epicsadata::add_definitions_to_bucket(country = country, station_id = station_id, new_definitions = new_definitions))
}
