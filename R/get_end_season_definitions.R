#' Get end of season definitions
#'
#' Retrieves end season definitions.
#'
#' @param end_season The end season data.
#' @return A list representation of end season definitions.
#' @examples
#' # Example usage:
#' #get_end_season_definitions(end_season)
get_end_season_definitions <- function(end_season = NULL){
  # Create an empty list
  data_list <- list()
  data_list[["end_season"]] <- list()
  variables_list <- c("start_day", "end_day", "water_balance_max",
                      "capacity", "evaporation", "evaporation_value")
  
  if (!is.null(end_season))  {
    start_day <- extract_value(end_season$filter_2, " >= ")
    end_day <- extract_value(end_season$filter_2, " <= ")
    output <- "both"
    water_balance_max <- extract_value(end_season$filter[[1]], "wb <= ")
    capacity <- extract_value(end_season$filter$wb$wb_max$rain_max[[2]], "yes=")
    evaporation_value <- extract_value(end_season$filter$wb$wb_max[[1]], "rain_max - ")
    if (is.na(evaporation_value)){
      evaporation_value <- extract_value(end_season$filter$wb$wb_max[[1]], "no=", FALSE)
      evaporation <- "variable"
    } else {
      evaporation <- "value"
    }
  }
  
  # Loop through variables and add to the list if defined
  for (variable in variables_list) {
    if (exists(variable) && !is.na(get(variable))) {
      data_list[["end_season"]][[variable]] <- get(variable)
    } else {
      data_list[["end_season"]][[variable]] <- NA
    }
  }
  return(data_list)
}