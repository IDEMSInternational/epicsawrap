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

#' Get Definitions Data (single-station)
#'
#' Retrieve definitions data for a single station OR directly via a definitions_id/file.
#' Chooses the most recent JSON in GCS when multiple versions exist.
#'
#' @param country character(1). Country code understood by `get_bucket_name()`.
#' @param station_id character(1) or NULL. If provided, fetch the station's latest definitions.
#' @param definitions_id character(1) or NULL. Stem like "ABC" (without .json). Used if station_id is NULL.
#' @param file character(1) or NULL. Exact file stem "ABC.20250101T010101Z" (without .json). Overrides others.
#' @return Parsed definitions object (list/data.frame depending on your JSON).
#' @export
get_definitions_data <- function(country, station_id = NULL, definitions_id = NULL, file = NULL) {
  if (length(country) != 1L) stop("'country' must be length 1", call. = FALSE)
  if (!is.null(station_id) && length(station_id) != 1L) {
    stop("'station_id' must be a single value (this function only supports one station).", call. = FALSE)
  }
  
  read_or_update <- function(id_stem) {
    path <- file.path("definitions", sprintf("%s.json", id_stem))
    if (file.exists(path)) jsonlite::read_json(path) else update_definitions_data(country, id_stem)
  }
  

  
  # --- main logic ------------------------------------------------------------
  # 1) Exact file overrides everything
  if (!is.null(file)) {
    return(read_or_update(file))
  }
  
  # 2) Station path (single station only)
  if (!is.null(station_id)) {
    station_id <- as.character(station_id)
    meta <- station_metadata(country = country, station_id = station_id)
    
    # Take the last metadata id for this station
    meta_ids <- meta$definitions_id[[1]]
    last_meta_id <- if (length(meta_ids)) meta_ids[length(meta_ids)] else NA_character_
    
    effective_id <- if (!is.null(definitions_id)) definitions_id else last_meta_id
    if (is.null(effective_id) || is.na(effective_id) || identical(effective_id, character(0))) {
      stop("No 'definitions_id' available for station '", station_id, "'.", call. = FALSE)
    }
    
    latest_id <- resolve_latest_for(country, effective_id)
    return(read_or_update(latest_id))
  }
  
  # 3) Station-agnostic: require a definitions_id (if no file and no station)
  if (is.null(definitions_id)) {
    stop("Provide either 'station_id', 'file', or 'definitions_id'.", call. = FALSE)
  }
  latest_id <- resolve_latest_for(country, definitions_id)
  read_or_update(latest_id)
}

#' Get Summaries Data
#'
#' @param country A character vector specifying the country or countries from which to get the data. Options are defined in `get_bucket_name()` (e.g., `"zm"`, `"mw"`).
#' @param station_id A character string specifying the ID of the station for which to get the summary data.
#' @param summary A character string specifying the summary to retrieve.
#'
#' @return A list of data frames containing the summary data for the specified stations and country.
#' @export
#'
#' @examples #
#' 
get_summaries_data <- function(country, station_id, summary) {
  if (length(country) > 1) stop("'country' must be of length 1")
  station_id <- as.character(station_id)
  dfs <- vector("list", length(station_id))
  names(dfs) <- station_id
  bucket_name <- get_bucket_name(country)
  for (i in seq_along(station_id)) {
    objects_in_space <- googleCloudStorageR::gcs_list_objects(bucket = bucket_name, prefix = paste0("summaries/", summary, "_", station_id[i], "."), versions = TRUE)
    
    if (nrow(objects_in_space) == 0){
      dfs[[i]] <- objects_in_space
      timestamp <- NULL
    } else {
      #   for rds_files > 1 (e.g., if several summary files)
      rds_files <- objects_in_space$name
      
      if (length(rds_files) > 1) {
        timestamps <- gsub(".*\\.(\\d+)\\.rds", "\\1", rds_files)
        timestamps <- suppressWarnings(as.numeric(timestamps))
        most_recent_index <- which.max(timestamps)
        rds_files <- rds_files[most_recent_index]
      }
        station_id[i] <- stringr::str_remove(stringr::str_remove(rds_files, paste0("summaries/", summary, "_")), ".rds")
      f <- paste0(country, "/", rds_files)
      if (file.exists(f)) {
        dfs[[i]] <- readRDS(f)
      } else {
        f <- update_summaries_data(country, station_id[i], summary)
        dfs[[i]] <- f
      }
      timestamp <- gsub(".*\\.(\\d+)\\.rds", "\\1", rds_files)
    }
  }
  return(list(dfs[[i]], timestamp))
}

#' List Definition Versions
#'
#' Retrieves all available versions of a given definition file stored in a
#' Google Cloud Storage (GCS) bucket for the specified country.
#'
#' This function lists all object versions whose names start with
#' `"definitions/{definition_id}."` and end with `.json`, allowing you to
#' track or access historical versions of a particular definition.
#'
#' @param country A character string specifying the country code (e.g. `"mz"`,
#' `"zm"`) used to determine the correct GCS bucket via `get_bucket_name()`.
#' @param definition_id A character string specifying the definition ID whose
#' versions should be listed.
#'
#' @return A character vector of object names (file paths) ending in `.json`
#' representing all available versions of the given definition in the bucket.
#' Returns an empty character vector if no matching objects are found.
#'
#' @examples
#' \dontrun{
#' list_definition_versions(country = "mz", definition_id = "soil_definitions")
#' }
#'
#' @seealso [get_bucket_name()], [googleCloudStorageR::gcs_list_objects()]
#' @export
list_definition_versions <- function(country, definition_id) {
  bucket <- get_bucket_name(country)
  
  objs <- googleCloudStorageR::gcs_list_objects(
    bucket = bucket,
    prefix = sprintf("definitions/%s.", definition_id),
    versions = TRUE
  )
  if (is.null(objs) || nrow(objs) == 0) character() else
    unname(objs$name[grepl("\\.json$", objs$name, perl = TRUE)])
}

#' Get Processed Station Metadata
#'
#' This function retrieves and processes station metadata for the specified country and format. The station metadata includes information about station IDs and their associated definitions.
#'
#' @param country A character vector specifying the country code for which station metadata should be retrieved and processed. Options are defined in `get_bucket_name()` (e.g., `"zm"`, `"mw"`).
#' @param station_id A character vector specifying the station ID(s) for the given country.
#' @param format A character vector indicating the desired format of the processed data. It can be "wide", "long", "nested", or "list".
#'
#' @return Depending on the specified format, the function returns the processed station metadata in either wide, long, nested, or list format.
#'
#' @export
#'
#' @examples
#' # Retrieve and process station metadata for country "zm" in wide format
#' #station_metadata_definitions(country = "zm", format = "wide")
#'
#' # Retrieve and process station metadata for countries "zm" and "mw" in long format
#' #station_metadata_definitions(country = c("zm", "mw"), format = "long")
#'
#' # Retrieve and process station metadata for country "zm" in nested format
#' #station_metadata_definitions(country = "zm", format = "nested")
station_metadata_definitions <- function(country, station_id, format = c("wide", "long", "nested", "list")){
  format <- match.arg(format)
  station_data <- station_metadata(country = country, station_id = station_id)
  
  if (format == "list"){
    definitions_id <- lapply(station_data$definitions_id, function(x) x[length(x)])
    result_list <- purrr::map2(.x = station_data$station_id,
                               .y = unlist(definitions_id),
                               .f = ~ c(station_data %>% filter(station_id == .x),
                                       list(data = get_definitions_data(country = country, .x, .y))))
    names(result_list) <- station_data$station_id
    return(result_list)
  } else {
    definitions_id <- lapply(station_data$definitions_id, function(x) x[length(x)])
    definitions_data <- purrr::map2(.x = station_data$station_id,
                                   .y = unlist(definitions_id),
                                   .f = ~ data.frame(station_id = .x, t(unlist(get_definitions_data(country = country, .x, .y)))))
    definitions_data <- dplyr::bind_rows(definitions_data)
    
    wide_df <- dplyr::full_join(station_data, definitions_data)
    
    if (format == "wide"){
      return(wide_df)
    } else {
      long_df <- wide_df %>% tidyr::pivot_longer(cols = !colnames(station_data), names_to = "definition", values_to = "value")
      if(format == "long"){
        return(long_df)
      } else {
        nested_df <- long_df %>% tidyr::nest(.by = colnames(station_data), data = c(definition, value))
        return(nested_df)
      }
    }
  }
}

#' Get Station Metadata
#'
#' This function retrieves metadata for weather stations in specified countries.
#'
#' @param country A character vector specifying the country or countries from which to get the metadata. Options are defined in `get_bucket_name()` (e.g., `"zm"`, `"mw"`).
#' @param station_id A character vector specifying the station IDs to filter by. If provided, only metadata for the specified station IDs will be returned.
#' @param include_definitions_id A logical value indicating whether to include the definitions id. If `TRUE`, definitions_id is given. 
#' @param include_definitions A logical value indicating whether to include definitions data. If `TRUE`, additional information about station definitions will be included in the output.
#' @param format A character vector specifying the format of the output. Options are `"wide"` (default), `"long"`, `"nested"`, or `"list"`.
#' @return If `include_definitions` is FALSE, the function returns a data frame with metadata for the specified stations. If `include_definitions` is `TRUE`, it returns a data frame with both metadata and station definitions.
#' @export
#'
#' @examples
#' # TODO
#' 
#' @seealso
#' \code{update_metadata} for updating metadata files.
#'
#' @importFrom purrr map list_rbind
#' @importFrom dplyr full_join filter mutate
#' @importFrom tidyr pivot_longer nest
station_metadata <- function(country = NULL, station_id = NULL, include_definitions_id = TRUE, include_definitions = FALSE, format = c("wide", "long", "nested", "list")){
  format <- match.arg(format)
  
  # if no country is given, then no station_id can be given
  if (is.null(country) && !is.null(station_id)) {
    warning("No country given. Ignoring station_id")
    station_id <- NULL
  }
  
  # If no country is given, then set it to be both countries.
  #  if (is.null(country)) { country <- c("mw", "zm") }
  if (is.null(country)) { 
    warning("No country given. ZM will be returned.")
    country <- c("zm")
  }
  # temporarily disabled because we do not have metadata or definition files for mw
  
  # Check if the metadata file exists, if not, update it
  check_exists <- function(f, country){
    if (file.exists(f)) {
      metadata <- readRDS(f)
    } else {
      f <- update_metadata(country)
      metadata <- f#readRDS(f)
    }
  }
  
  # Create the file path for metadata. Then check the file exists, if not, update it.
  station_data <- purrr::map(.x = country,
                             .f = ~ check_exists(f = paste0(.x, "/", "metadata", ".rds"),
                                                 country = .x) %>%
                               dplyr::mutate(country_code = country))
  
  if (length(station_data) == 1) station_data <- station_data[[1]] 
  # Filter by 'station_id' if provided
  if (!is.null(station_id)){
    station_id_vars <- station_id
    station_data <- station_data %>%
      dplyr::filter(station_id %in% station_id_vars)
  }
  
  if (!include_definitions_id){
    vars_to_return <- c("station_id", "station_name", "latitude", "longitude", "elevation", "district", "country_code")
    vars_to_return <- vars_to_return[vars_to_return %in% names(station_data)]
    station_data <- station_data %>% dplyr::select(all_of(vars_to_return))
  }
  if (!include_definitions) return(station_data)
  
  # if include definitions then run the following -
  #  if (is.data.frame(station_data)){
  station_data <- station_metadata_definitions(country, station_id, format = format)
  # } else {
  #   station_data <- purrr::map(.x = station_data, .f = ~station_metadata_definitions(.x, format = format))
  # }
  return(station_data) 
}

#' Get the Definitions Data
#' 
#' @description This function updates the definitions data for a specific station in the specified country. It retrieves the data from Google Cloud Storage using the `get_data` function.
#'
#' @param country A character vector specifying the country or countries from which to update the data. Options are defined in `get_bucket_name()` (e.g., `"zm"`, `"mw"`).
#' @param station_id A character string specifying the ID of the station for which to update the daily data.
#' 
#' @details 
#' The `update_definitions` function is used to update the daily data for a specific station in the specified country. It internally calls the `get_data` function to retrieve the data from Google Cloud Storage.
#' The `country` argument is a character vector that allows specifying one or more countries from which to update the data. The data will be updated for Mozambique (`"mz"`) and Zambia (`"zm"`). You can modify this argument to update data for different countries.
#' The `station_id` argument is a character string that specifies the ID of the station for which to update the daily data. The function will construct the filename by concatenating the `"definitions/"` directory, the `station_id`, and the `file` extension `".json"`. The filename will be passed to the `get_data` function to retrieve the data.
#' The function uses the invisible function to suppress the output of the `get_data` function, ensuring that the data retrieval process is not visible in the console.
#'
#' @return This function does not return any value explicitly. It gets the daily data for the specified station in the specified country.
update_definitions_data <- function(country, station_id) {
  filename <- paste0("definitions", "/", station_id, ".json")
  invisible(get_data(country = country, filename = filename))
}

#' Update Station Metadata (add/update and optionally remove stations)
#'
#' @param country Character scalar (e.g. "zm", "mw").
#' @param metadata_data Optional data frame of new/updated metadata. Default NULL for remove-only runs.
#' @param station_var Optional name of station id column in `metadata_data`. Required if `metadata_data` is provided.
#' @param latitude_var,longitude_var,elevation_var,district_var Optional names in `metadata_data`.
#' @param remove_stations Optional character vector of station ID (canonical) to remove after updates.
#' @param allow_new Logical; if FALSE, do not add stations that don't already exist. Default TRUE.
#' @return Updated metadata data frame (also uploaded).
#' @export
update_metadata_info <- function(country,
                                 metadata_data = NULL,
                                 station_var = NULL,
                                 latitude_var = NULL,
                                 longitude_var = NULL,
                                 elevation_var = NULL,
                                 district_var = NULL,
                                 remove_stations = NULL,
                                 allow_new = TRUE) {
  bucket <- get_bucket_name(country)
  complete_metadata_from_bucket <- station_metadata(country)
  
  # standardise canonical name column
  if (!"station_name" %in% names(complete_metadata_from_bucket)) {
    stop("Existing metadata must contain a 'station_name' column.")
  }
  complete_metadata_from_bucket <- complete_metadata_from_bucket |>
    dplyr::mutate(station_name = trimws(station_name))
  
  # prepare incoming data (if provided)
  if (!is.null(metadata_data)) {
    if (is.null(station_var)) stop("`station_var` must be provided when `metadata_data` is supplied.")
    # collect only columns we need
    selected_vars <- c(station_var, latitude_var, longitude_var, elevation_var, district_var)
    selected_vars <- selected_vars[!vapply(selected_vars, is.null, logical(1))]
    
    md <- metadata_data |>
      dplyr::select(dplyr::any_of(selected_vars)) |>
      dplyr::rename_with(~"station_name", dplyr::all_of(station_var))
    
    if (!is.null(latitude_var))  md <- dplyr::rename_with(md, ~"latitude",  dplyr::all_of(latitude_var))
    if (!is.null(longitude_var)) md <- dplyr::rename_with(md, ~"longitude", dplyr::all_of(longitude_var))
    if (!is.null(elevation_var)) md <- dplyr::rename_with(md, ~"elevation", dplyr::all_of(elevation_var))
    if (!is.null(district_var))  md <- dplyr::rename_with(md, ~"district",  dplyr::all_of(district_var))
    
    md <- md |>
      dplyr::mutate(station_name = trimws(station_name)) |>
      dplyr::filter(!is.na(station_name)) |>
      unique()
    
    # if (anyDuplicated(md$station_name)) {
    #   stop("`metadata_data` contains duplicate `station_name` rows; please deduplicate before calling.")
    # }
    
    # choose join kind based on allow_new
    join_fun <- if (isTRUE(allow_new)) dplyr::full_join else dplyr::left_join
    
    updated_metadata <- join_fun(complete_metadata_from_bucket, md, by = "station_name")
    
    # coalesce only for supplied fields
    if (!is.null(latitude_var)  && all(c("latitude.x","latitude.y")   %in% names(updated_metadata))) {
      updated_metadata <- updated_metadata |>
        dplyr::mutate(latitude = dplyr::coalesce(.data$latitude.y, .data$latitude.x)) |>
        dplyr::select(-dplyr::any_of(c("latitude.x","latitude.y")))
    }
    if (!is.null(longitude_var) && all(c("longitude.x","longitude.y") %in% names(updated_metadata))) {
      updated_metadata <- updated_metadata |>
        dplyr::mutate(longitude = dplyr::coalesce(.data$longitude.y, .data$longitude.x)) |>
        dplyr::select(-dplyr::any_of(c("longitude.x","longitude.y")))
    }
    if (!is.null(elevation_var) && all(c("elevation.x","elevation.y") %in% names(updated_metadata))) {
      updated_metadata <- updated_metadata |>
        dplyr::mutate(elevation = dplyr::coalesce(.data$elevation.y, .data$elevation.x)) |>
        dplyr::select(-dplyr::any_of(c("elevation.x","elevation.y")))
    }
    if (!is.null(district_var)  && all(c("district.x","district.y")   %in% names(updated_metadata))) {
      updated_metadata <- updated_metadata |>
        dplyr::mutate(district = dplyr::coalesce(.data$district.y, .data$district.x)) |>
        dplyr::select(-dplyr::any_of(c("district.x","district.y")))
    }
    
    # if allow_new = FALSE, ensure no brand-new stations sneaked in
    if (!isTRUE(allow_new)) {
      updated_metadata <- updated_metadata |>
        dplyr::semi_join(complete_metadata_from_bucket, by = "station_name")
    }
    
  } else {
    # remove-only run
    updated_metadata <- complete_metadata_from_bucket
  }
  
  # apply removals last so they always take effect
  if (!is.null(remove_stations) && length(remove_stations)) {
    remove_stations <- trimws(remove_stations)
    not_found <- setdiff(remove_stations, updated_metadata$station_name)
    if (length(not_found)) {
      message("Stations not found (not removed): ", paste(not_found, collapse = ", "))
    }
    updated_metadata <- updated_metadata |>
      dplyr::filter(!(.data$station_id) %in% remove_stations)
  }
  
  # reorder to original schema where possible
  updated_metadata <- updated_metadata |>
    dplyr::select(dplyr::any_of(names(complete_metadata_from_bucket)),
                  dplyr::everything())
  
  # upload
  object_function <- function(input, output) saveRDS(input, file = output)
  googleCloudStorageR::gcs_upload(
    file = updated_metadata,
    bucket = bucket,
    name = "metadata.rds",
    object_function = object_function,
    predefinedAcl = "bucketLevel"
  )
  
  updated_metadata
}

