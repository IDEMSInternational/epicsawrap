
#' Update Metadata Definition ID
#'
#' This function updates the definitions ID in the station metadata for a given country and station ID. 
#' It can either overwrite the existing definitions ID or append a new one.
#'
#' @param country A character string representing the country code.
#' @param station_id A character string representing the station ID.
#' @param definition_id A character string representing the new definition ID to be added.
#' @param overwrite A logical value indicating whether to overwrite the existing definitions ID. 
#' @param add_climsoft A logical value indicating whether to add in climsoft details. This will add in the values from `elementfiltercolumn` and `elements`.
#' @param elementfiltercolumn Name of the column to filter by elements, default is 'elementName'.
#' @param elements Vector of element IDs to filter the data.
#' If \code{TRUE}, the existing definitions ID will be overwritten. If \code{FALSE}, the new 
#' definition ID will be appended. Default is \code{FALSE}.
#'
#' @return None. The function updates the metadata in the specified cloud storage bucket.
#' 
#' @export
update_metadata_definition_id <- function(country, station_id, definition_id, overwrite = FALSE,
                                          add_climsoft = FALSE,
                                          elementfiltercolumn = "elementName",
                                          elements = c("Temp  Daily Max", "Temp  Daily Min", "Precip  Daily")) {
  bucket <- get_bucket_name(country)
  station_id_names <- station_id
  complete_metadata_from_bucket <- station_metadata(country)
  reference <- complete_metadata_from_bucket$station_id
  complete_metadata_from_bucket_filt <- complete_metadata_from_bucket %>% dplyr::filter(station_id %in% station_id_names)
  if (nrow(complete_metadata_from_bucket_filt) > 0){
    # add definition ID
    complete_metadata_from_bucket_rest <- complete_metadata_from_bucket %>% dplyr::filter(!station_id %in% station_id_names)
    if (!is.list(complete_metadata_from_bucket_rest$definitions_id)) complete_metadata_from_bucket_rest$definitions_id <- as.list(complete_metadata_from_bucket_rest$definitions_id)
    if (overwrite){
      complete_metadata_from_bucket_filt$definitions_id <- definition_id
    } else {
      complete_metadata_from_bucket_filt$definitions_id <- purrr::map(.x = complete_metadata_from_bucket_filt$definitions_id, .f = ~ unique(c(.x, definition_id)))
    }
    complete_metadata_from_bucket <- dplyr::bind_rows(complete_metadata_from_bucket_rest, complete_metadata_from_bucket_filt)
    complete_metadata_from_bucket <- complete_metadata_from_bucket[order(match(complete_metadata_from_bucket$station_id, reference)),]
  } else {
    new_df <- data.frame(station_id = station_id_names)
    new_df$definitions_id <- purrr::map(.x = definition_id, .f = ~ unique(c(.x)))
    if (add_climsoft){
      climsoft_list <- list(elementfiltercolumn, elements)
      names(climsoft_list) <- c("elementName", "elements")
      new_df$climsoft_list <- list(climsoft_list)
    }
    complete_metadata_from_bucket <- dplyr::bind_rows(complete_metadata_from_bucket, new_df)
  }
  object_function <- function(input, output) { saveRDS(input, file = output) }
  googleCloudStorageR::gcs_upload(file = complete_metadata_from_bucket, bucket = bucket, name = "metadata.rds", object_function = object_function, predefinedAcl = "bucketLevel")
}

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

#' Add New Definitions to Google Cloud Storage Bucket
#'
#' This function adds a new definitions file with a timestamp to a specified Google Cloud Storage (GCS) bucket. The new definitions are sourced from a JSON file provided as input.
#'
#' @param country A character vector specifying the country or countries from which to get the definitions data. Options are `"mz"` and `"zm"`.
#' @param definitions_id A character string specifying the ID of the definitions data.
#' @param new_definitions A character vector specifying the path to the JSON file containing the new definitions.
#' @param timestamp A character vector with a timestamp. By default this is `NULL` so is generated.
#'
#' @details
#' The function creates a timestamp in the format "YYYYMMDDHHMMSS" and appends it to the definitions_id to form the filename. It then reads the provided JSON file, creates a new JSON file with the timestamped filename, and uploads it to the specified GCS bucket.
#'
#' @importFrom googleCloudStorageR gcs_upload
#' @importFrom jsonlite read_json
#'
#' @seealso
#' \code{get_bucket_name} for retrieving the GCS bucket name.
#'
add_definitions_to_bucket <- function(country, definitions_id, new_definitions, timestamp = NULL){
  bucket <- get_bucket_name(country)
  definitions_dir <- "definitions"
  
  # Create a timestamp if there isn't one already
  if (is.null(timestamp)) {
    timestamp <- format(Sys.time(), format = "%Y%m%d%H%M%S")
    timestamp <- paste0(".", timestamp)
  }
  
  # Define the filename with the timestamp 
  new_filename <- paste0(definitions_id, ".", timestamp, ".json")
  
  object_function <- function(input, output) {
    jsonlite::write_json(input, path = output, auto_unbox = TRUE, pretty = TRUE)
  }
  googleCloudStorageR::gcs_upload(file = new_definitions, bucket = bucket, 
                                  name = paste0(file.path(definitions_dir, new_filename)),
                                  object_function = object_function, 
                                  predefinedAcl = "bucketLevel")
}
