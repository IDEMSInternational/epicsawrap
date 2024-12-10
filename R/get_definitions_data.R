#' Get Definitions Data
#'
#' This function retrieves definitions data for weather stations or specific definitions IDs from a Google Cloud Storage (GCS) bucket.
#' It includes logic to handle station-based retrieval or fetch data directly using a specified `definitions_id`. It also handles 
#' timestamp management to ensure the most recent definitions file is imported.
#'
#' @param country A character vector of length 1 specifying the country from which to get the definitions data.
#'                Options depend on the implementation of `get_bucket_name()`, with common options being `"mz"` (Mozambique)
#'                and `"zm"` (Zambia).
#' @param station_id A character vector specifying the ID(s) of the station(s) for which to get the definitions data. 
#'                   If `NULL`, data is fetched using the `definitions_id`. Defaults to `NULL`.
#' @param definitions_id A character string specifying the ID of the definitions to retrieve. If `NULL` and `station_id` is provided,
#'                       the most recent definitions ID is determined from metadata. Defaults to `NULL`.
#' @param file A character string specifying the name of a specific file to import, in the format `"STATIONNAME.TIMESTAMP"`. 
#'             If `NULL`, the most recent definitions file is fetched automatically. Defaults to `NULL`.
#'
#' @return A data frame or list containing the definitions data:
#' - If `station_id` is provided, returns data specific to the station(s).
#' - If `station_id` is `NULL`, returns data specific to the `definitions_id`.
#'
#' @details
#' - When `station_id` is provided, the function fetches the corresponding definitions data for each station.
#' - If `station_id` is `NULL`, the function directly retrieves data based on the provided `definitions_id`.
#' - The function uses Google Cloud Storage to retrieve the files, ensuring that the most recent versions are accessed when `file` is `NULL`.
#' - For multiple stations, the function returns a combined data frame.
#'
#' @export
get_definitions_data <- function(country, station_id = NULL, definitions_id = NULL, file = NULL) {
  if (length(country) > 1) stop("'country' must be of length 1")
  if (is.null(station_id)){
    # return the data for that definitions ID
    bucket_name <- get_bucket_name(country)
    
    files <- googleCloudStorageR::gcs_list_objects(bucket = bucket_name,
                                                   prefix = paste0("definitions/", definitions_id, "."),
                                                   versions = TRUE)
    
    if (nrow(files) == 0) { stop("No files found. Check country and station_id")}
    # Filter files with the ".json" extension
    files <- files %>% dplyr::filter(grepl("\\.json$", name))
    json_files <- files$name
    
    # Check if multiple json files found. If so, take hte most recent one.
    if (length(json_files) >= 1) definitions_id <- extract_most_recent_json(json_files)
    f <- paste0("definitions/", definitions_id, ".json")
    if (file.exists(f)) {
      definitions_data <- jsonlite::read_json(f)
    } else {
      f <- update_definitions_data(country, definitions_id)
      definitions_data <- f #jsonlite::write_json(f)
    }
    return(definitions_data)
  } else {
    station_id <- as.character(station_id)
    dfs <- vector("list", length(station_id))
    names(dfs) <- station_id
    
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
        if (length(json_files) >= 1){
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
}
