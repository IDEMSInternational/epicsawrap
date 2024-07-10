#' Get start of rains definitions
#'
#' Retrieves start rains definitions.
#'
#' @param start_rains The start rains data.
#' @return A list of start of rains definitions
#'
#' @examples
#' # Example usage:
#' #get_start_rains_definitions(start_rains)
get_start_rains_definitions <- function(start_rains = NULL){
  # Create an empty list
  data_list <- list()
  data_list[["start_rains"]] <- list()
  # Create a list
  variables_list = c("start_day", "end_day", "threshold", "total_rainfall", 
                     "over_days", "amount_rain", "proportion", "prob_rain_day", 
                     "dry_spell", "spell_max_dry_days", "spell_interval", 
                     "dry_period", "max_rain", "period_interval", "period_max_dry_days")
  
  if (!is.null(start_rains)) {
    start_day <- extract_value(start_rains$filter_2, " >= ")
    end_day <- extract_value(start_rains$filter_2, " <= ")
    output <- "both"
    
    # Important! Assuming that threshold is the first argument!
    threshold <- extract_value(start_rains$filter[[1]], " >= ")
    
    # if null, then we didn't run it so set that to be false in definitions file.
    if (is.null(start_rains$filter$roll_sum_rain)){
      total_rainfall <- FALSE  
    } else {
      total_rainfall <- TRUE
      if (is.null(start_rains$filter$wet_spell)){
        amount_rain <- extract_value(start_rains$filter[[1]], "roll_sum_rain > ")
        over_days <- extract_value(start_rains$filter$roll_sum_rain[[2]], "n=")
        proportion <- FALSE
      } else {
        prob_rain_day <- extract_value(start_rains$filter$wet_spell[[1]], "probs=")
        over_days <- extract_value(start_rains$filter$wet_spell$roll_sum_rain[[2]], "n=")
        proportion <- TRUE
      }
    }
    if (is.null(start_rains$filter$roll_n_rain_days)){
      number_rain_days <- FALSE 
    } else {
      number_rain_days <- TRUE
      min_rain_days <- extract_value(start_rains$filter[[1]], "roll_n_rain_days >= ")
      rain_day_interval <- extract_value(start_rains$filter$roll_n_rain_days[[1]], "n=")
    }
    if (is.null(start_rains$filter$roll_max_dry_spell)){
      dry_spell <- FALSE
    } else {
      dry_spell <- TRUE
      spell_max_dry_days <- extract_value(start_rains$filter[[1]], "roll_max_dry_spell <= ")
      spell_interval <- extract_value(start_rains$filter$roll_max_dry_spell[[1]], "n=")
    }
    if (is.null(start_rains$filter$n_dry_period)){
      dry_period <- FALSE
    } else {
      dry_period <- TRUE
      max_rain <- extract_value(start_rains$filter$n_dry_period[[1]], "roll_sum_rain_dry_period <= ")
      period_interval <- extract_value(start_rains$filter$n_dry_period[[1]], "n=")
      period_max_dry_days <- extract_value(start_rains$filter$n_dry_period[[1]],
                                           paste0("n=", period_interval, " - "))
    }
  }
  # Loop through each variable in the list
  for (variable in variables_list) {
    # Check if the variable exists and is not NA
    if (exists(variable) && !is.na(get(variable))) {
      # Retrieve the variable's value
      variable_value <- get(variable)
      
      # Check if the variable's class includes "instat_calculation"
      if ("instat_calculation" %in% class(variable_value)) {
        data_list[["start_rains"]][[variable]] <- NA
      } else {
        data_list[["start_rains"]][[variable]] <- variable_value
      }
    } else {
      # Assign NA if the variable does not exist or is NA
      data_list[["start_rains"]][[variable]] <- NA
    }
  }
  return(data_list)
}
