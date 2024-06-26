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
  bucket <- epicsadata:::get_bucket_name(country)
  station_id_names <- station_id
  complete_metadata_from_bucket <- epicsadata::station_metadata(country)
  reference <- complete_metadata_from_bucket$station_id
  complete_metadata_from_bucket_filt <- complete_metadata_from_bucket %>% dplyr::filter(station_id %in% station_id_names)
  if (nrow(complete_metadata_from_bucket_filt) > 0){
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