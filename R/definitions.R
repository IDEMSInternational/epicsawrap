#' Definitions
#'
#' @param country `character(1)` The country code of the data.
#' @param definitions_id `character(1)` The definitions code in the data.
#' @param station_id `character(1)` The definitions code in the data.
#' @param summaries `character` Vector of summaries to display
#' @param file Default `NULL` meaning that the most recent definitions file will be found and imported. Otherwise specify as a string the file to import. In format: "STATIONNAME.TIMESTAMP" e.g. "1.20240311152831"
#'
#' @return TODO
#' @export
#'
#' @examples # e.g. definitions("zm", "16", "annual_rain")
#' # error: definitions("zm", "1", c("annual_rain", "hi", "end_season"))
definitions <- function(country, station_id = NULL, definitions_id = NULL, summaries, file = NULL){
  definition_data <- get_definitions_data(country = country, station_id = station_id, file = file)
  definition_data <- purrr::map(.x = summaries, .f = ~ definition_data[[.x]])
  names(definition_data) <- summaries
  # are any NULL 1 = NULL
  null_check <- purrr::map_dbl(.x = summaries, .f = ~ is.null(definition_data[[.x]]))
  if (any(null_check == 1)){
    warning(paste0("Not all summaries are defined in the json definition file: ", paste(x = names(definition_data)[which(null_check == 1)], collapse = ", ")))
  }
  definition_data[sapply(definition_data, is.null)] <- NULL
  return(definition_data)
}
