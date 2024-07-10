#' Summarize Rain Definitions
#'
#' This function summarises rain definitions for a specific time period.
#'
#' @param time A character string specifying the time period for which the rain definitions are summarised (`"annual_rain"` or `"seasonal_rain""`).
#' @param total_rain Logical indicating whether total rain is considered.
#' @param n_rain Logical indicating whether the number of rainy days is considered.
#' @param sum_rain Numeric vector containing the sum of rainfall.
#' @param n_raindays Numeric vector containing the number of rainy days.
#' @param data Optional additional data (default `NULL`).
#' 
#' @return A list containing summarised rain definitions for the specified time period.
#'
#' @examples
#'# TODO
sum_rain_definitions <- function(time = "annual_rain", total_rain,
         n_rain, sum_rain,
         n_raindays, data = NULL){
  data_list <- list()
  data_list[[time]] <- list()
  if (n_rain){
    rain_day <- extract_value(data$count$rain_day[[2]], " >= ", FALSE)
  }
  sum_rain <- c(sum_rain, n_raindays)
  
  na_rm <- extract_value(sum_rain$function_exp, "na.rm = ", FALSE)
  na_n <- extract_value(sum_rain$function_exp, "na_max_n = ", TRUE)
  na_n_non <- extract_value(sum_rain$function_exp, "na_min_n = ", TRUE)
  na_consec <- extract_value(sum_rain$function_exp, "na_consecutive_n = ", TRUE)
  na_prop <- extract_value(sum_rain$function_exp, "na_max_prop = ", TRUE)
  
  variables_list = c("total_rain", "n_rain", "rain_day", "na_rm",
                     "na_n", "na_n_non", "na_consec", "na_prop")
  
  # Create an empty list
  data_list <- list()
  data_list[[time]] <- list()
  
  # Loop through variables and add to the list if defined
  for (variable in variables_list) {
    if (exists(variable) && !is.na(get(variable))) {
      data_list[[time]][[variable]] <- get(variable)
    } else {
      data_list[[time]][[variable]] <- NA
    }
  }
  return(data_list)
}
