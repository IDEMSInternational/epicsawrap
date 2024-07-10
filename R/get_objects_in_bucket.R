#' Import Summary Definitions
#' 
#' This function imports summary definitions based on the country, station ID, summaries, and get_summaries.
#' It checks if there are any files corresponding to the provided station and summary in the Google Cloud Storage bucket.
#' If files are found, it imports definitions from the file; otherwise, it generates new definitions.
#' 
#' @param country A character string specifying the country.
#' @param station_id A character string specifying the station ID.
#' @param timestamp The timestamp on the object file name to import. Default `NULL`
#' 
#' @return A list containing imported or generated summary definitions.
#' 
#' @importFrom googleCloudStorageR gcs_list_objects
#' 
#' @examples
#' # Import summary definitions
#' #import_summary_definitions("USA", "station123", list("info1", "info2"))
#' 
#' @export
get_objects_in_bucket <- function(country, station_id, timestamp) {
  if (!is.null(timestamp)){
    file_name <- paste0(station_id, ".", timestamp)
  } else {
    file_name <- station_id
  }
  bucket_name <- get_bucket_name(country)
  files <- googleCloudStorageR::gcs_list_objects(bucket = bucket_name,
                                                 prefix = paste0("definitions/", file_name, "."),
                                                 versions = TRUE)
  return(files)
}