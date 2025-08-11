#' Get temperature summaries
#'
#' Retrieves temperature summaries based on provided parameters.
#'
#' @param temp_summary_name Character vector specifying the name of the temperature summary.
#' @param data A list of temperature summaries by definition (e.g., year or month).
#' @return A list containing temperature summary information.

#' @examples
#' # Example usage:
get_temp_summaries <- function(temp_summary_name, data, to = c("annual", "monthly")){
  
  temp_summary_name_list <- NULL
  variables_list = c("to", "na_rm", "na_n", "na_n_non", "na_consec", "na_prop")
  
  # if there is neither, return the definitions in an empty file
  if (is.null(data)){
    for (variable in variables_list) {
      temp_summary_name_list[[variable]] <- NA
    }
  } else {
    # Note, we take the na.rm bits from data_by_year
    temp_summary <- data[[temp_summary_name]]
    temp_summary_2 <- data[[temp_summary_name]]
    if (!is.null(temp_summary)){
      to <- to
      na_rm <- extract_value(temp_summary$function_exp, "na.rm = ", FALSE)
      na_n <- extract_value(temp_summary$function_exp, "na_max_n = ", TRUE)
      na_n_non <- extract_value(temp_summary$function_exp, "na_min_n = ", TRUE)
      na_consec <- extract_value(temp_summary$function_exp, "na_consecutive_n = ", TRUE)
      na_prop <- extract_value(temp_summary$function_exp, "na_max_prop = ", TRUE)
      for (variable in variables_list) {
        if (exists(variable) && all(!is.na(get(variable)))) {
          temp_summary_name_list[[variable]] <- get(variable)
        } else {
          temp_summary_name_list[[variable]] <- NA
        }
      }
    } else {
      temp_summary <- temp_summary_2
      temp_summary$by_1 <- temp_summary$by_2
      for (variable in variables_list) {
        temp_summary_name_list[[variable]] <- NA
      }
    } 
  }
  return(temp_summary_name_list)
}