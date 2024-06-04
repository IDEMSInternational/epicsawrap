#' Get Definitions ID from Metadata
#'
#' This function retrieves the definitions ID from station metadata for a given country and station ID.
#'
#' @param country A character string representing the country code.
#' @param station_id A character string representing the station ID.
#'
#' @return A character string representing the definitions ID from the station metadata.
#' 
#' @export
get_definitions_id_from_metadata <- function(country, station_id) {
  station_id_metadata <- epicsadata::station_metadata(country = country, station_id = station_id, include_definitions = FALSE)
  if (nrow(station_id_metadata) == 0) {
    warning(paste0(station_id, " not found in metadata. No definition ID given. Returning station_id."))
    definitions_id <- station_id
  } else {
    definitions_id <- station_id_metadata$definitions_id[1]
  }
  return(definitions_id)
}
