#' Get annual rain definitions
#'
#' Retrieves annual rain definitions.
#'
#' @param data_name The name of the main data frame.
#' @param data_by_year The definitions file for the data_by_year data frame.
#' @param rain_name The rain variable in the data frame.
#' @return A list representation of annual rain definitions.
#' 
#' @examples
#' # Example usage:
#' #get_total_rain_counts(annual_rain, ghana_defs)
#' 
get_total_rain_counts <- function(data_name,
                                  data_by_year,
                                  rain_name = data_book$get_climatic_column_name(data_name = "ghana", col_name = "rain")){
  # get seasonal_sum_rain as the definition even if there are multiple.
  # and annual_sum_rain even if multiple
  # This is found by seeing which are
  # called sum_<rainfallname>, and then checking if there is filter_by_year for seasonal
  sum_rain_name <- paste0("sum_", rain_name)
  data_by_year_names <- names(data_by_year)
  count_names <- grepl(sum_rain_name, data_by_year_names)
  annual_total_rain <- "FALSE"
  annual_sum_rain <- NULL
  seasonal_total_rain <- "FALSE"
  seasonal_sum_rain <- NULL
  
  # TODO: get to work for counting the number of rainy days in season
  seasonal_count_raindays <- "FALSE"
  seasonal_n_raindays <- NULL
  annual_count_raindays <- "FALSE"
  annual_n_raindays <- NULL
  
  if (sum(count_names) > 0){
    sum_rain_lists <- which(count_names == TRUE)
    for (i in sum_rain_lists){
      sum_rain_i <- data_by_year[[i]]
      # check if there is filtering - if it is filtered based on start and end rains then it is seasonal.
      if (any(grepl("filter", names(sum_rain_i)))){
        if (grepl("start_rain", sum_rain_i$filter_2) && grepl("end_", sum_rain_i$filter_2)){
          seasonal_total_rain <- "TRUE"
          seasonal_sum_rain <- sum_rain_i
        }
        # if there is no filtering, then it is annual.
      } else {
        annual_total_rain <- "TRUE"
        annual_sum_rain <- sum_rain_i
      }
    }
  }
  
  # for counting the number of rainy days (sum_Rainday)
  count_names <- grepl("sum_Rainday", data_by_year_names)
  if (sum(count_names) > 0){
    sum_rain_lists <- which(count_names == TRUE)
    for (i in sum_rain_lists){
      sum_rain_i <- data_by_year[[i]]
      # check if there is filtering - if it is filtered based on start and end rains then it is seasonal.
      if (any(grepl("filter", names(sum_rain_i)))){
        if (grepl("start_rain", sum_rain_i$filter_2) && grepl("end_", sum_rain_i$filter_2)){
          seasonal_count_raindays <- "TRUE"
          seasonal_n_raindays <- sum_rain_i
        }
        # if there is no filtering, then it is annual.
      } else {
        annual_count_raindays <- "TRUE"
        annual_n_raindays <- sum_rain_i
      }
    }
  }
  
  # todo: how to ensure we're getting the correct count.
  if (annual_count_raindays){
    data_definition <- get_r_instat_definitions(data_book$get_calculations(data_name))
  } else {
    if (seasonal_count_raindays){
      data_definition <- get_r_instat_definitions(data_book$get_calculations(data_name))
    } else {
      data_definition <- NULL
    }
  }
  
  if (as.logical(annual_total_rain) | as.logical(annual_count_raindays)){
    annual_rain_definition <- sum_rain_definitions(time = "annual_rain", total_rain = annual_total_rain,
                                                   n_rain = annual_count_raindays, sum_rain = annual_sum_rain,
                                                   n_raindays = annual_n_raindays, data = data_definition)
  }
  
  if (as.logical(seasonal_total_rain) | as.logical(seasonal_count_raindays)){
    seasonal_rain_definition <- sum_rain_definitions(time = "seasonal_total_rainfall", total_rain = seasonal_total_rain,
                                                     n_rain = seasonal_count_raindays, sum_rain = seasonal_sum_rain,
                                                     n_raindays = seasonal_n_raindays, data = data_definition)
  }
  
  rain_definitions <- c(annual_rain_definition, seasonal_rain_definition)
  return(rain_definitions)
}


sum_rain_definitions <- function(time = "annual_rain", total_rain = annual_total_rain,
                                 n_rain = annual_count_raindays, sum_rain = annual_sum_rain,
                                 n_raindays = annual_n_raindays, data = NULL){
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
    }
  }
  return(data_list)
}