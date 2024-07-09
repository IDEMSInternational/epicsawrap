# TODO: save_to should only be there for the forecast data 
#' Get data from Google Cloud Storage
#' @description This function retrieves data from Google Cloud Storage for a specified country and file. The data can either be parsed and returned as an R object or saved to a local disk.
#'
#' @param country A character string specifying the country. 
#' @param filename A character string specifying the name of the file in Google Cloud Storage. 
#' @param save_to (Optional) A character string specifying the local path where the file should be saved. If not provided, the data will be parsed and returned as an R object.
#'
#' @details 
#' The `get_data` function retrieves data from a specified country and file stored in Google Cloud Storage. It uses the `googleCloudStorageR` package
#' If the `save_to` argument is not provided or set to `NULL`, the function will parse the data and return it as an R object using the `googleCloudStorageR::gcs_parse_rds` function. This is useful when you want to directly work with the data in R, and can be more efficient.
#' If the `save_to` argument is provided with a valid local path, the function will save the file to the specified location on the disk using the `saveToDisk` parameter. This is useful when you want to download the file for further processing or analysis outside of R.
#'
#' @return The function returns the retrieved data as an R object if `save_to` is `NULL.` If `save_to` is provided, the function saves the data locally.
#' @export
#'
#' @examples # TODO
get_data <- function(country, filename, save_to = NULL) {
  bucket <- get_bucket_name(country)
  if (is.null(save_to)){
    if (substr(filename, nchar(filename)-4+1, nchar(filename)) == ".rds"){
      googleCloudStorageR::gcs_get_object(object_name = filename, bucket = bucket,
                                          parseFunction = googleCloudStorageR::gcs_parse_rds)
    } else {
      googleCloudStorageR::gcs_get_object(object_name = filename, bucket = bucket,
                                          parseFunction = googleCloudStorageR::gcs_parse_download) 
    }
  } else {
    googleCloudStorageR::gcs_get_object(object_name = filename, bucket = bucket,
                                        saveToDisk = save_to) 
  }
}
