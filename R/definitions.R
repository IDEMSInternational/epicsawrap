#' Definitions
#'
#' @param country `character(1)` The country code of the data.
#' @param station_id `character(1)` The station code in the data.
#' @param summaries `character` Vector of summaries to display
#'
#' @return TODO
#' @export
#'
#' @examples #TODO
definitions <- function(country, station_id, summaries){
  definition_data <- epicsadata::get_definitions_data(country = country, station_id = station_id)
  definition_data <- purrr::map(.x = summaries, .f = ~ definition_data[[.x]])
  names(definition_data) <- summaries
  return(definition_data)
}