#' Calculate total temperature summaries
#'
#' Calculates total temperature summaries based on provided parameters.
#'
#' @param data_by_year A list of temperature summaries by definition (e.g., year).
#' @param data_by_year_month An optional second list of temperature summaries by definition (e.g., year and month).
#' @param min_tmin_column The name of the minimum of minimum temperature column in the data.
#' @param max_tmin_column The name of the maximum of minimum temperature column in the data.
#' @param mean_tmin_column The name of the mean of minimum temperature column in the data.
#' @param min_tmax_column The name of the minimum of maximum temperature column in the data.
#' @param max_tmax_column The name of the maximum of maximum temperature column in the data.
#' @param mean_tmax_column The name of the mean of maximum temperature column in the data.
#' @param min_monthly_tmin_column Column name for minimum of minimum temperatures (for monthly temperature data).
#' @param max_monthly_tmin_column Column name for maximum of minimum temperatures (for monthly temperature data).
#' @param mean_monthly_tmin_column Column name for mean of minimum temperatures (for monthly temperature data).
#' @param min_monthly_tmax_column Column name for minimum of maximum temperatures (for monthly temperature data).
#' @param max_monthly_tmax_column Column name for maximum of maximum temperatures (for monthly temperature data).
#' @param mean_monthly_tmax_column Column name for mean of maximum temperatures (for monthly temperature data).
#' @return A list containing total temperature summaries.
#' 
#' @examples
#' # Example usage:
#' #total_temperature_summaries(tmin = "tmin", tmax = "tmax",
#' #                             data_by_year = my_definition_list, data_by_year_month = my_definition_list_2)
build_total_temperature_summaries <- function(data_by_year,
                                              data_by_year_month,
                                              min_tmin_column,
                                              mean_tmin_column,
                                              max_tmin_column,
                                              min_tmax_column,
                                              mean_tmax_column,
                                              max_tmax_column,
                                              min_monthly_tmin_column,
                                              mean_monthly_tmin_column,
                                              max_monthly_tmin_column,
                                              min_monthly_tmax_column,
                                              mean_monthly_tmax_column,
                                              max_monthly_tmax_column){
  build_block <- function(min_col, mean_col, max_col, var, data, to) {
    cols <- c(min = min_col, mean = mean_col, max = max_col)
    purrr::map(cols, ~ get_temp_summaries(.x, data, to = to)) |>
      purrr::set_names(paste0(names(cols), "_", var))
  }
  
  annual <- c(
    build_block(min_tmin_column, mean_tmin_column, max_tmin_column,
                "tmin", data_by_year, "annual"),
    build_block(min_tmax_column, mean_tmax_column, max_tmax_column,
                "tmax", data_by_year, "annual")
  )
  
  monthly <- c(
    build_block(min_monthly_tmin_column, mean_monthly_tmin_column, max_monthly_tmin_column,
                "tmin", data_by_year_month, "monthly"),
    build_block(min_monthly_tmax_column, mean_monthly_tmax_column, max_monthly_tmax_column,
                "tmax", data_by_year_month, "monthly")
  )
  
  data_list <-  list(annual = annual, monthly = monthly)
  
  names(data_list) <- c("annual_temperature_summaries",
                        "monthly_temperature_summaries")
  
  return(data_list)
}
