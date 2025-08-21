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
      dplyr::filter(!is.na(station_name))
    
    if (anyDuplicated(md$station_name)) {
      stop("`metadata_data` contains duplicate `station_name` rows; please deduplicate before calling.")
    }
    
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
