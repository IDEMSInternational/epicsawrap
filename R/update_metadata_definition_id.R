#' Update Metadata Definition ID
#'
#' This function updates the definitions ID in the station metadata for a given country and station ID. 
#' It can either overwrite the existing definitions ID or append a new one.
#'
#' @param country A character string representing the country code.
#' @param station_id A character string representing the station ID.
#' @param definition_id A character string representing the new definition ID to be added.
#' @param overwrite A logical value indicating whether to overwrite the existing definitions ID. 
#' If \code{TRUE}, the existing definitions ID will be overwritten. If \code{FALSE}, the new 
#' definition ID will be appended. Default is \code{FALSE}.
#'
#' @return None. The function updates the metadata in the specified cloud storage bucket.
#' 
#' @export
update_metadata_definition_id <- function(country, station_id, definition_id, overwrite = FALSE) {
  bucket <- get_bucket_name(country)
  get_metadata_from_bucket <- epicsadata:::station_metadata(country, station_id)
  
  if (overwrite) {
    get_metadata_from_bucket$definitions_id <- definition_id
  } else {
    get_metadata_from_bucket$definitions_id <-
      purrr::map(.x = get_metadata_from_bucket$definitions_id,
                 .f = ~ c(.x, definition_id))
  }
  
  object_function <- function(input, output) {
    saveRDS(input, file = output)
  }
  
  googleCloudStorageR::gcs_upload(file = get_metadata_from_bucket,
                                  bucket = bucket, 
                                  name = "metadata.rds", 
                                  object_function = object_function,
                                  predefinedAcl = "bucketLevel")
}
