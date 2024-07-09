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
