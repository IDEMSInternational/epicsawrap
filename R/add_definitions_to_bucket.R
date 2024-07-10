#' Add New Definitions to Google Cloud Storage Bucket
#'
#' This function adds a new definitions file with a timestamp to a specified Google Cloud Storage (GCS) bucket. The new definitions are sourced from a JSON file provided as input.
#'
#' @param country A character vector specifying the country or countries from which to get the definitions data. Options are `"mz"` and `"zm"`.
#' @param station_id A character string specifying the ID of the station for which to get the definitions data.
#' @param new_definitions A character vector specifying the path to the JSON file containing the new definitions.
#' @param timestamp A character vector with a timestamp. By default this is `NULL` so is generated.
#'
#' @details
#' The function creates a timestamp in the format "YYYYMMDDHHMMSS" and appends it to the station_id to form the filename. It then reads the provided JSON file, creates a new JSON file with the timestamped filename, and uploads it to the specified GCS bucket.
#'
#' @export
#' @importFrom googleCloudStorageR gcs_upload
#' @importFrom jsonlite read_json
#'
#' @seealso
#' \code{get_bucket_name} for retrieving the GCS bucket name.
#'
add_definitions_to_bucket <- function(country, station_id, new_definitions, timestamp = NULL){
  bucket <- get_bucket_name(country)
  definitions_dir <- "definitions"
  
  # Create a timestamp if there isn't one already
  if (is.null(timestamp)) {
    timestamp <- format(Sys.time(), format = "%Y%m%d%H%M%S")
    timestamp <- paste0(".", timestamp)
  }
  
  # Define the filename with the timestamp 
  new_filename <- paste0(station_id, ".", timestamp, ".json")
  
  object_function <- function(input, output) {
    jsonlite::write_json(input, path = output, auto_unbox = TRUE, pretty = TRUE)
  }
  googleCloudStorageR::gcs_upload(file = new_definitions, bucket = bucket, 
                                  name = paste0(file.path(definitions_dir, new_filename)),
                                  object_function = object_function, 
                                  predefinedAcl = "bucketLevel")
}