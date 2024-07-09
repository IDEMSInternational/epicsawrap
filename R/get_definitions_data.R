#' Get Daily Definitions Data
#'
#' This function retrieves definitions data for weather stations from a Google Cloud Storage (GCS) bucket. It includes timestamp handling to ensure that the most recent definitions file is imported.
#'
#' @param country A character vector specifying the country or countries from which to get the definitions data. Options are any defined in `get_bucket_name()`. Common options are `"mz"` and `"zm"`.
#' @param station_id A character string specifying the ID of the station for which to get the definitions data.
#' @param definitions_id A character string specifying the ID of the definitions for which to get the definitions data. If `NULL` this is found from the metadata.
#' @param file Default `NULL` meaning that the most recent definitions file will be found and imported. Otherwise specify as a string the file to import. In format: "STATIONNAME.TIMESTAMP" e.g. "1.20240311152831"
#'
#' @return A data frame containing daily data based on the station ID.
#'
#' @importFrom googleCloudStorageR gcs_list_objects gcs_get_object
#' @importFrom jsonlite fromJSON
#'
#' @seealso
#' \code{update_definitions_data} for updating definitions files.
#'
#' @export
#'
#' @examples # todo
get_definitions_data <- function(country, station_id, definitions_id = NULL, file = NULL) {
  if (length(country) > 1) stop("'country' must be of length 1")
  station_id <- as.character(station_id)
  dfs <- vector("list", length(station_id))
  names(dfs) <- station_id
  
  # for (i in seq_along(station_id)) {
  #   f <- paste0(country, "/", "definitions", "/", station_id[i], ".json")
  #   if (file.exists(f)) {
  #     dfs[[i]] <- jsonlite::read_json(f)
  #   } else {
  #     f <- update_definitions_data(country, station_id[i])
  #     dfs[[i]] <- f #jsonlite::write_json(f)
  #   }
  # }
  # TODO: set up so if (is.null(definitions_id)) runs station_data, otherwise we cll it through from params
  station_data <- station_metadata(country = country, station_id = station_id)
  definitions_id_list <- lapply(station_data$definitions_id, function(x) x[length(x)])
  names(definitions_id_list) <- station_id
  
  if (is.null(file)){
    bucket_name <- get_bucket_name(country)
    for (i in seq_along(station_id)) {
      if (is.null(definitions_id)) definitions_id <- definitions_id_list[[i]]
      # List all files in the "definitions" directory for the station
      files <- googleCloudStorageR::gcs_list_objects(bucket = bucket_name,
                                                     prefix = paste0("definitions/", definitions_id, "."),
                                                     versions = TRUE)
      
      if (nrow(files) == 0) { stop("No files found. Check country and station_id")}
      # Filter files with the ".json" extension
      files <- files %>% dplyr::filter(grepl("\\.json$", name))
      json_files <- files$name
      
      # Check if multiple json files found. If so, take hte most recent one.
      if (length(json_files) > 1){
        # Extract timestamps from file names
        definitions_id[i] <- extract_most_recent_json(json_files)
      }
      f <- paste0("definitions/", definitions_id[i], ".json")
      if (file.exists(f)) {
        dfs[[i]] <- jsonlite::read_json(f)
      } else {
        f <- update_definitions_data(country, definitions_id[i])
        dfs[[i]] <- f #jsonlite::write_json(f)
      }
    }
    if (length(station_id) > 1) {
      station_data <- dplyr::bind_rows(dfs)
    } else {
      station_data <- dfs[[1]]
    }
  } else {
    f <- paste0("definitions/", file, ".json")
    if (file.exists(f)) {
      station_data <- jsonlite::read_json(f)
    } else {
      f <- update_definitions_data(country, file)
      station_data <- f
    }
  }
  return(station_data)
}
