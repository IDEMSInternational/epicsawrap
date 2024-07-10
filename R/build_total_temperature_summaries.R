#' Calculate total temperature summaries
#'
#' Calculates total temperature summaries based on provided parameters.
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
#' @return A list containing total temperature summaries.
#' 
#' @examples
#' # Example usage:
#' #total_temperature_summaries(tmin = "tmin", tmax = "tmax", year = "year", month = "month", 
#' #                             data_by_year = my_definition_list, data_by_year_month = my_definition_list_2)
build_total_temperature_summaries <- function(year = data_book$get_climatic_column_name(data_name, "year"),
                                              month = data_book$get_climatic_column_name(data_name, "month"),
                                              data_by_year,
                                              data_by_year_month,
                                              min_tmin_column, mean_tmin_column, max_tmin_column,
                                              min_tmax_column, mean_tmax_column, max_tmax_column){
  data_list <- get_temperature_summary_definitions(year = year, month = month,
                                                   data_by_year = data_by_year,
                                                   data_by_year_month = data_by_year_month,
                                                   min_tmin_column = min_tmin_column, 
                                                   mean_tmin_column = mean_tmin_column, 
                                                   max_tmin_column = max_tmin_column,
                                                   min_tmax_column = min_tmax_column, 
                                                   mean_tmax_column = mean_tmax_column, 
                                                   max_tmax_column = max_tmax_column)  
  return(data_list)
}
