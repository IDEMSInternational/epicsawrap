#' GCS Auth with json file
#'
#' This function authenticates the user for Google Cloud Storage (GCS) using a JSON key file.
#' 
#' @param filename The path to the JSON key file for GCS authentication.
#' 
#' @return None
#' @export
#' 
#' @examples # gcs_auth_file("path/to/key.json")
#'
#' @references
#' For more information on GCS authentication, refer to the official documentation:
#'   https://cloud.google.com/storage/docs/reference/libraries#client-libraries-usage-r
#'
#' @importFrom googleCloudStorageR gcs_auth
gcs_auth_file <- function(filename) {
  googleCloudStorageR::gcs_auth(filename)
}