definitions <- function(country, station_id, summaries){
  definition_data <- epicsadata::get_definitions_data(country = country, station_id = station_id)
  definition_data <- purrr::map(.x = summaries, .f = ~ definition_data[[.x]])
  names(definition_data) <- summaries
  return(definition_data)
}