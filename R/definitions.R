#' Definitions
#'
#' @param country `character(1)` The country code of the data.
#' @param station_id `character(1)` The station code in the data.
#' @param summaries `character` Vector of summaries to display
#'
#' @return TODO
#' @export
#'
#' @examples # e.g. definitions("zm", "16", "annual_rain")
#' # error: definitions("zm", "1", c("annual_rain", "hi", "end_season"))
definitions <- function(country, station_id, summaries){
  definition_data <- epicsadata::get_definitions_data(country = country, station_id = station_id)
  definition_data <- purrr::map(.x = summaries, .f = ~ definition_data[[.x]])
  names(definition_data) <- summaries
  # are any NULL 1 = NULL
  null_check <- purrr::map_dbl(.x = summaries, .f = ~ is.null(definition_data[[.x]]))
  if (any(null_check == 1)){
    warning(paste0("Not all summaries are defined in the json definition file: ",
                paste(x = names(definition_data)[which(null_check == 1)], collapse = ", ")))
  }
  definition_data[sapply(definition_data, is.null)] <- NULL
  return(definition_data)
}
