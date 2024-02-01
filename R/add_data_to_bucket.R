#' Add New Data to Google Cloud Storage Bucket
#'
#' This function adds a new data file (RDS format) with a timestamp to a specified Google Cloud Storage (GCS) bucket.
#'
#' @param country A character vector specifying the country that the data corresponds to. Options are `"mz"` and `"zm"`.
#' @param station_id A character string specifying the ID of the station that the data corresponds to.
#' @param data The data to upload.
#'
#' @details
#' The function creates a timestamp in the format "YYYYMMDDHHMMSS" and appends it to the station_id to form the filename. It then reads the provided file, creates a new file with the timestamped filename, and uploads it to the specified GCS bucket.
#'
#' @export
add_data_to_bucket <- function(country, station_id, data){
  return(epicsadata::add_data_to_bucket(country = country, station_id = station_id, data = data))
}