#' Add New Definitions to Google Cloud Storage Bucket
#'
#' This function adds a new definitions file with a timestamp to a specified Google Cloud Storage (GCS) bucket. The new definitions are sourced from a JSON file provided as input.
#'
#' @param country A character vector specifying the country that the data corresponds to. Options are `"mz"` and `"zm"`.
#' @param station_id A character string specifying the ID of the station that the data corresponds to.
#' @param new_definitions A character vector specifying the path to the JSON file containing the new definitions.
#' @param timestamp A character vector with a timestamp. By default this is `NULL` so is generated at the time of creation.
#'
#' @export
#' @details
#' The function creates a timestamp in the format "YYYYMMDDHHMMSS" and appends it to the station_id to form the filename. It then reads the provided JSON file, creates a new JSON file with the timestamped filename, and uploads it to the specified GCS bucket.
add_definitions_to_bucket <- function (country, station_id, new_definitions, timestamp = NULL) 
{
  bucket <- get_bucket_name(country)
  definitions_dir <- "definitions"
  if (is.null(timestamp)) {
    timestamp <- format(Sys.time(), format = "%Y%m%d%H%M%S")
  }
  new_filename <- paste0(station_id, ".", timestamp, 
                         ".json")
  #new_json <- paste0(new_definitions, ".json")
  # googleCloudStorageR::gcs_upload(file = new_json, bucket = bucket, 
  #                                 name = file.path(definitions_dir, new_filename))
  # 
  object_function <- function(input, output) {
    jsonlite::write_json(input, path = output, auto_unbox = TRUE, pretty = TRUE)
  }
  googleCloudStorageR::gcs_upload(file = new_definitions, bucket = bucket, 
                                  name = paste0(file.path(definitions_dir, new_filename)),
                                  object_function = object_function, 
                                  predefinedAcl = "bucketLevel")
}