#' Get temperature summaries
#'
#' Retrieves temperature summaries based on provided parameters.
#'
#' @param temp_summary_name Character vector specifying the name of the temperature summary.
#' @param year Numeric vector specifying the year.
#' @param month Numeric vector specifying the month.
#' @param data_by_year A list of temperature summaries by definition (e.g., year).
#' @param data_by_year_month An optional second list of temperature summaries by definition (e.g., year and month).
#' @return A list containing temperature summary information.

#' @examples
#' # Example usage:
#' #get_temp_summaries("summary_name", year = 2023, month = 5, data_by_year = my_definition_list)
get_temp_summaries <- function(temp_summary_name, year, month,
                               data_by_year, data_by_year_month = NULL){
  
  temp_summary_name_list <- NULL
  variables_list = c("to", "na_rm", "na_n", "na_n_non", "na_consec", "na_prop")
  
  # if there is neither, return the definitions in an empty file
  if (is.null(data_by_year) && is.null(data_by_year_month)){
    for (variable in variables_list) {
      temp_summary_name_list[[variable]] <- NA
    }
  } else {
    # Note, we take the na.rm bits from data_by_year
    temp_summary <- data_by_year[[temp_summary_name]]
    temp_summary_2 <- data_by_year_month[[temp_summary_name]]
    if (is.null(temp_summary)){
      temp_summary <- temp_summary_2
      temp_summary$by_1 <- temp_summary$by_2
    }
    to <- c()
    if (!is.null(temp_summary)){
      if (year %in% unlist(temp_summary[grep("^by_", names(temp_summary))]) | year %in% unlist(temp_summary_2[grep("^by_", names(temp_summary_2))])){
        to <- c(to, "annual")
      }
      if (month %in% unlist(temp_summary[grep("^by_", names(temp_summary))]) | month %in% unlist(temp_summary_2[grep("^by_", names(temp_summary_2))])){
        to <- c(to, "monthly")
      }
      na_rm <- extract_value(temp_summary$function_exp, "na.rm = ", FALSE)
      na_n <- extract_value(temp_summary$function_exp, "na_max_n = ", TRUE)
      na_n_non <- extract_value(temp_summary$function_exp, "na_min_n = ", TRUE)
      na_consec <- extract_value(temp_summary$function_exp, "na_consecutive_n = ", TRUE)
      na_prop <- extract_value(temp_summary$function_exp, "na_max_prop = ", TRUE)
      for (variable in variables_list) {
        if (exists(variable) && !is.na(get(variable))) {
          temp_summary_name_list[[variable]] <- get(variable)
        } else {
          temp_summary_name_list[[variable]] <- NA
        }
      }
    } else {
      for (variable in variables_list) {
        temp_summary_name_list[[variable]] <- NA
      }
    } 
  }
  return(temp_summary_name_list)
}