#' Get end rains definitions
#'
#' Retrieves end rains definitions.
#'
#' @param end_rains The end rains data.
#' @return A list representation of end rains definitions.
#' @examples
#' # Example usage:
#' #get_end_rains_definitions(end_rains)
get_end_rains_definitions <- function(end_rains = NULL){
  # Create an empty list
  data_list <- list()
  data_list[["end_rains"]] <- list()
  variables_list = c("start_day", "end_day",  "output", "min_rainfall", "interval_length")
  if (!is.null(end_rains)) {
    start_day <- extract_value(end_rains$filter_2, " >= ")
    end_day <- extract_value(end_rains$filter_2, " <= ")
    output <- "both"
    min_rainfall <- extract_value(end_rains$filter[[1]], "roll_sum_rain > ")
    if (is.null(end_rains$filter$roll_sum_rain[[2]])){
      stop("No roll_sum_rain value found. Have you put end_rains_column to equal 'end_season'? You want end_season_column to equal 'end_season'.")
    }
    interval_length <- extract_value(end_rains$filter$roll_sum_rain[[2]], "n=")
  }
  # Loop through variables and add to the list if defined
  for (variable in variables_list) {
    if (exists(variable) && !is.na(get(variable))) {
      data_list[["end_rains"]][[variable]] <- get(variable)
    } else {
      data_list[["end_rains"]][[variable]] <- NA
    }
  }
  
  return(data_list)
}