#' Update PDF/JPEG Data
#' 
#' @description This function updates the PDF/JPEG data for a specific station in the specified country. It retrieves the data from Google Cloud Storage using the `get_data` function.
#'
#' @param country A character vector specifying the country or countries from which to update the data. Options are `"mz"` and `"zm"`.
#' @param station_id A character string specifying the ID of the station for which to update the daily data.
#' @param type A character string specifying whether the data to retrieve is JPEG or PDF.
#' 
#' @details 
#' The `country` argument is a character vector that allows specifying one or more countries from which to get the PDF/JPEG data. The data will be updated for Mozambique (`"mz"`) and Zambia (`"zm"`). You can modify this argument to update data for different countries.
#' The `station_id` argument is a character string that specifies the ID of the station for which to update the PDF/JPEG data. The function will construct the filename by concatenating the `"pdf/"` or `"jpeg"/` directory, the `station_id`, and the `file` extension. The filename will be passed to the `get_data` function to retrieve the data.
#' The function uses the invisible function to suppress the output of the `get_data` function, ensuring that the data retrieval process is not visible in the console.
#'
#' @return This function does not return any value explicitly. It gets the PDF/JPEG data for the specified station in the specified country.
#' @export
#'
#' @examples # get_binary_file("zm", "16", "pdf")
get_binary_file <- function(country = c("mz", "zm"), station_id, type = c("pdf", "jpeg")) {
  type <- match.arg(type)
  filename <- paste0(type, "/", station_id, ".", type)
  invisible(get_data(country = country, filename = filename))
}

#' Get Forecast Data
#'
#' @param country A character vector specifying the country or countries from which to get the forecast data. Options are defined in `get_bucket_name()`.
#' @param station_id A character string specifying the ID of the station for which to get the forecast data.
#'
#' @return todo
#' @export
#'
#' @examples # todo
get_forecast_data <- function(country, station_id) {
  # and anything else we need here! I've just taken this from update_daily_data.R
  filename <- paste0("data", "/", station_id, ".rds")
  saveto <- paste0(country, "/", filename)
  invisible(get_data(country = country, filename = filename, save_to = saveto))
}

#' Add New Data to Google Cloud Storage Bucket
#'
#' This function adds a new data file (RDS format) with a timestamp to a specified Google Cloud Storage (GCS) bucket.
#'
#' @param country A character vector specifying the country that the data corresponds to. Options are `"mz"` and `"zm"`.
#' @param station_id A character string specifying the ID of the station that the data corresponds to.
#' @param data The data to upload.
#' @param timestamp A character vector with a timestamp. By default this is `NULL` so is generated.
#'
#' @details
#' The function creates a timestamp in the format "YYYYMMDDHHMMSS" and appends it to the station_id to form the filename. It then reads the provided file, creates a new file with the timestamped filename, and uploads it to the specified GCS bucket.
#'
#' @importFrom googleCloudStorageR gcs_upload
#' @importFrom jsonlite read_json
#'
#' @seealso
#' \code{get_bucket_name} for retrieving the GCS bucket name.
#'
add_data_to_bucket <- function(country, station_id, data, timestamp = NULL) {
  # Set the GCS bucket name
  bucket <- get_bucket_name(country)
  data_dir <- "data"
  
  if (is.null(timestamp)){
    # Generate a timestamp
    timestamp <- format(Sys.time(), format = "%Y%m%d%H%M%S")
  }
  
  # Define the filename with the station ID, timestamp, and RDS extension
  new_filename <- paste0(station_id, ".", timestamp, ".rds")
  
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