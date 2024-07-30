#' Update Station Metadata
#'
#' This function updates the metadata for specified stations in a given country by merging new data from `metadata_data` with existing metadata.
#'
#' @param country A character vector specifying the country or countries from which to update the metadata. Options are defined in `get_bucket_name()` (e.g., `"zm"`, `"mw"`).
#' @param station_id A character vector specifying the station IDs to update.
#' @param metadata_data A data frame containing new metadata for the stations. This data should include columns for station identifiers and other metadata fields like latitude, longitude, etc.
#' @param station_var Optional. The name of the column in `metadata_data` corresponding to station IDs. Defaults to `NULL`.
#' @param latitude_var Optional. The name of the column in `metadata_data` containing latitude values. Defaults to `NULL`.
#' @param longitude_var Optional. The name of the column in `metadata_data` containing longitude values. Defaults to `NULL`.
#' @param elevation_var Optional. The name of the column in `metadata_data` containing elevation values. Defaults to `NULL`.
#' @param district_var Optional. The name of the column in `metadata_data` containing district information. Defaults to `NULL`.
#'
#' @return A data frame with updated metadata for the specified stations.
#' @export
#'
#' @examples
#' # Assuming get_bucket_name and station_metadata functions are defined:
#' # update_metadata_info("zm", c("BINGA", "KARIBA"), new_metadata_data, 
#' #                     station_var = "stationName", latitude_var = "lat", longitude_var = "lon")
update_metadata_info <- function(country, station_id,
                                 metadata_data,
                                 station_var = NULL,
                                 latitude_var = NULL,
                                 longitude_var = NULL,
                                 elevation_var = NULL,
                                 district_var = NULL) {
  bucket <- get_bucket_name(country)
  complete_metadata_from_bucket <- station_metadata(country)

  # Filter out NULL values from the variable list
  selected_vars <- c(station_var, latitude_var, longitude_var, elevation_var, district_var)
  selected_vars <- selected_vars[!sapply(selected_vars, is.null)]
  
  # Select and rename columns from metadata_data if necessary
  metadata_data <- metadata_data %>%
    select(any_of(selected_vars))
  metadata_data <- metadata_data %>% rename_with(~ "station_id", station_var)
  if (!is.null(latitude_var)) metadata_data <- metadata_data %>% dplyr::rename_with(~ "latitude", latitude_var)
  if (!is.null(longitude_var)) metadata_data <- metadata_data %>% dplyr::rename_with(~ "longitude", longitude_var)
  if (!is.null(elevation_var)) metadata_data <- metadata_data %>% dplyr::rename_with(~ "elevation", elevation_var)
  if (!is.null(district_var)) metadata_data <- metadata_data %>% dplyr::rename_with(~ "district", district_var)
    
  updated_metadata <- complete_metadata_from_bucket %>% dplyr::full_join(metadata_data, by = "station_id")
  
  # Join the dataframes on station_id
  # Conditionally mutate and select columns based on their presence
  if (!is.null(latitude_var)) {
    updated_metadata <- updated_metadata %>%
      dplyr::mutate(latitude = coalesce(latitude.y, latitude.x)) %>%
      dplyr::select(-c(latitude.x, latitude.y))
  }
  if (!is.null(longitude_var)) {
    updated_metadata <- updated_metadata %>%
      dplyr::mutate(longitude = coalesce(longitude.y, longitude.x)) %>%
      dplyr::select(-c(longitude.x, longitude.y))
  }
  if (!is.null(elevation_var)) {
    updated_metadata <- updated_metadata %>%
      dplyr::mutate(elevation = coalesce(elevation.y, elevation.x)) %>%
      dplyr::select(-c(elevation.x, elevation.y))
  }
  if (!is.null(district_var)) {
    updated_metadata <- updated_metadata %>%
      dplyr::mutate(district = coalesce(district.y, district.x)) %>%
      dplyr::select(-c(district.x, district.y))
  }
  
  updated_metadata <- updated_metadata %>% dplyr::select(names(complete_metadata_from_bucket))
  
  object_function <- function(input, output) { saveRDS(input, file = output) }
  googleCloudStorageR::gcs_upload(file = updated_metadata, bucket = bucket, name = "metadata.rds", object_function = object_function, predefinedAcl = "bucketLevel")
}