#' Get temperature summary definitions
#'
#' Retrieves temperature summary definitions based on provided parameters.
#'
#' @param year Character vector specifying the year.
#' @param month Character vector specifying the month.
#' @param data_by_year A list of temperature summaries by definition (e.g., year).
#' @param data_by_year_month An optional second list of temperature summaries by definition (e.g., year and month).
#' @param min_tmin_column The name of the minimum of minimum temperature column in the data.
#' @param max_tmin_column The name of the maximum of minimum temperature column in the data.
#' @param mean_tmin_column The name of the mean of minimum temperature column in the data.
#' @param min_tmax_column The name of the minimum of maximum temperature column in the data.
#' @param max_tmax_column The name of the maximum of maximum temperature column in the data.
#' @param mean_tmax_column The name of the mean of maximum temperature column in the data.
#'
#' @return A list containing temperature summary definitions.
#' @export
#' 
#' @examples
#' # Example usage:
#' #get_temperature_summary_definitions(by_definition_list = my_definition_list, by_definition_2_list = my_definition_list_2)
get_temperature_summary_definitions <- function(year = data_book$get_climatic_column_name(data_name, "year"),
                                                month = data_book$get_climatic_column_name(data_name, "month"),
                                                data_by_year,
                                                data_by_year_month = NULL,
                                                min_tmin_column, mean_tmin_column, max_tmin_column,
                                                min_tmax_column, mean_tmax_column, max_tmax_column){
    tmin_summary_names <- paste0(c(min_tmin_column, mean_tmin_column, max_tmin_column))
    tmax_summary_names <- paste0(c(min_tmax_column, mean_tmax_column, max_tmax_column))
    temp_summary_names <- c(tmin_summary_names, tmax_summary_names)
  temp_summary_definitions <- purrr::map(.x = temp_summary_names,
                                         .f = ~get_temp_summaries(.x, year, month, data_by_year, data_by_year_month))
  names(temp_summary_definitions) <- c(paste0(c("min_", "max_", "mean_"), "tmin"),
                                       paste0(c("min_", "max_", "mean_"), "tmax"))
  return(temp_summary_definitions)
}
