#' Get total rain counts.
#'
#' This function retrieves the definition for the total rainfall as well as the
#' total number of rainy days for both annual and seasonal periods.
#'
#' @param data_name Character string specifying the name of the data.
#' @param data_by_year List containing data by year.
#' @param rain_name Character string specifying the name of the rainfall data.
#'
#' @return A list containing definitions of total rain counts for annual and seasonal periods.
#'
#' @examples
#' #data <- your_data
#' #get_total_rain_counts(data_name = "ghana", data_by_year = data, rain_name = "rain")
get_total_rain_counts <- function(data_name,
                                  data_by_year = NULL,
                                  rain_name = data_book$get_climatic_column_name(data_name = "ghana", col_name = "rain")){
  variables_list = c("total_rain", "n_rain", "rain_day", "na_rm",
                     "na_n", "na_n_non", "na_consec", "na_prop")
  annual_rain_definition <- NULL
  seasonal_rain_definition <- NULL
  
  if (is.null(data_by_year)){
    for (variable in variables_list) {
      annual_rain_definition[["annual_rain"]][[variable]] <- NA
    }
    for (variable in variables_list) {
      seasonal_rain_definition[["seasonal_rain"]][[variable]] <- NA
    }
  } else {
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
    
    # TODO: get to work for counting the number of rainy days.
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
    } else {
      for (variable in variables_list) {
        annual_rain_definition[["annual_rain"]][[variable]] <- NA
      }
    }
    
    if (as.logical(seasonal_total_rain) | as.logical(seasonal_count_raindays)){
      seasonal_rain_definition <- sum_rain_definitions(time = "seasonal_rain", total_rain = seasonal_total_rain,
                                                        n_rain = seasonal_count_raindays, sum_rain = seasonal_sum_rain,
                                                        n_raindays = seasonal_n_raindays, data = data_definition)
    } else {
      for (variable in variables_list) {
        seasonal_rain_definition[["seasonal_rain"]][[variable]] <- NA
      }
    }
  }
  
  rain_definitions <- c(annual_rain_definition, seasonal_rain_definition)
  return(rain_definitions)
}