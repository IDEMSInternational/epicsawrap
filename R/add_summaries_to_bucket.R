#' Add New Summaries to Google Cloud Storage Bucket
#'
#' This function adds a new summary data file (RDS format) with a timestamp to a specified Google Cloud Storage (GCS) bucket.
#'
#' @param country A character vector specifying the country that the data corresponds to. Options are `"mz"` and `"zm"`.
#' @param station_id A character string specifying the ID of the station that the data corresponds to.
#' @param data The data to upload.
#' @param summary The summary function used to create the data.
#' @param timestamp A character vector with a timestamp. By default this is `NULL` so is generated.
#'
#' @details
#' The function creates a timestamp in the format "YYYYMMDDHHMMSS" and appends it to the station_id to form the filename. It then reads the provided file, creates a new file with the timestamped filename, and uploads it to the specified GCS bucket.
#'
#' @export
#' @importFrom googleCloudStorageR gcs_upload
#' @importFrom jsonlite read_json
#'
#' @seealso
#' \code{get_bucket_name} for retrieving the GCS bucket name.
#'
#'
add_summaries_to_bucket <- function(country, station_id, data, summary, timestamp = NULL) {
  # Set the GCS bucket name
  bucket <- get_bucket_name(country)
  data_dir <- "summaries"
  
  if (is.null(timestamp)){
    # Generate a timestamp
    timestamp <- format(Sys.time(), format = "%Y%m%d%H%M%S")
  }
  
  # Define the filename with the station ID, timestamp, and RDS extension
  new_filename <- paste0(summary, "_", station_id, ".", timestamp, ".rds")
  
  # Custom function to write R object to RDS
  object_function <- function(input, output) {
    saveRDS(input, file = output)
  }
  
  # Upload the RDS file to the GCS bucket
  googleCloudStorageR::gcs_upload(file = data,
                                  bucket = bucket,
                                  name = paste0(file.path(data_dir, new_filename)),
                                  object_function = object_function,
                                  predefinedAcl = "bucketLevel")
}