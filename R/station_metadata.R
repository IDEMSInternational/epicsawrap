#' Get Station Metadata
#'
#' This function retrieves metadata for weather stations in specified countries.
#' @param country A character vector specifying the country or countries from which to get the metadata. Options include "zm" (Zambia) and "mz" (Mozambique).
#' @param station_id A character vector specifying the station IDs to filter by. If provided, only metadata for the specified station IDs will be returned.
#' @param include_definitions_id A logical value indicating whether to include the definitions ID.
#' @param include_definitions A logical value indicating whether to include definitions data. If TRUE, additional information about station definitions will be included in the output.
#' @param format A character vector specifying the format of the output. Options are "wide" (default), "long", "nested", or "list".
#' @return If `include_definitions` is FALSE, the function returns a data frame with metadata for the specified stations. If `include_definitions` is TRUE, it returns a data frame with both metadata and station definitions.
#' 
#' @export
station_metadata <- function(country = NULL, station_id = NULL, include_definitions_id = TRUE, include_definitions = FALSE, format = c("wide", "long", "nested", "list")) {
  epicsadata::station_metadata(country = country, station_id = station_id, include_definitions_id = include_definitions_id, include_definitions = include_definitions, format = format)
}
