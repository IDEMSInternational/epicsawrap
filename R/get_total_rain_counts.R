#' Get total rain counts.
#'
#' This function retrieves the definition for the total rainfall as well as the
#' total number of rainy days for both annual and seasonal periods.
#'
#' @param data_by_year List containing data by year.
#' @param rain_name Character string specifying the name of the rainfall data.
#'
#' @return A list containing definitions of total rain counts for annual and seasonal periods.
#'
#' @examples
#' #data <- your_data
#' #get_total_rain_counts(data_name = "ghana", data_by_year = data, rain_name = "rain")
get_total_rain_counts <- function(data_by_year = NULL,
                                  rain_name = data_book$get_climatic_column_name(data_name = "ghana", col_name = "rain")){
  
  # issue in getting seasonal_rain -- because we run the check for seasonal rain:
  # need to read in that start_rain column name, for one, 
  # but also is this even the right code?
  # grepl("start_rain", sum_rain_i$filter_2) && grepl("end_", sum_rain_i$filter_2)

  variables_list = c("total_rain", "n_rain", "rain_day", "na_rm",
                     "na_n", "na_n_non", "na_consec", "na_prop")
  annual_rain_definition <- NULL
  seasonal_rain_definition <- NULL
  
  # set as FALSE, to be set as TRUE if they're present
  annual_total_rain <- "FALSE"
  seasonal_total_rain <- "FALSE"
  seasonal_count_raindays <- "FALSE"
  annual_count_raindays <- "FALSE"
  seasonal_n_raindays <- NULL
  annual_n_raindays <- NULL
  
  # if we aren't calling in data_by_year, then set everything as NA
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
    
    # which of data_by_year are "sum_<rainfall>" -----------------------------
    # these are going to be potentially annual_rain$total_rain
    # and seasonal_rain$total_rain
    # and $na_rm $na_n $na_n_non $na_consec $na_prop
    count_names <- grepl(sum_rain_name, data_by_year_names)
    
    # so for sum_rainfall columns - the cols counting the rainfall:
    if (sum(count_names) > 0){
      sum_rain_lists <- which(count_names == TRUE)
      for (i in sum_rain_lists){
        sum_rain_i <- data_by_year[[i]]
        
        # check if there is filtering - if it is filtered based on start and end rains then it is seasonal.
        # TODO: this isn't true. fix this. 
        if (any(grepl("filter", names(sum_rain_i)))){
          if (grepl("start_rain", sum_rain_i$filter_2) && grepl("end_", sum_rain_i$filter_2)){
            seasonal_total_rain <- "TRUE"
            seasonal_sum_rain <- sum_rain_i
          } else if (grepl(">= 1", sum_rain_i$filter_2) && grepl("<= 36", sum_rain_i$filter_2)){
            annual_total_rain <- "TRUE"
            annual_sum_rain <- sum_rain_i
          }
          # if there is no filtering, then it is annual.
        } else {
          annual_total_rain <- "TRUE"
          annual_sum_rain <- sum_rain_i
        }
      }
    }
    
    # which of data_by_year are "sum_rainday" -----------------------------
    sum_rainday_col <- c("sum_rainday")
    # todo: is it always call sum_rainday? Presumably that "rainday" name comes from whatever it is called on the main raw data frame, with "sum_" in front of it. 
    # need to check and fix that. 
    
    # these are going to be potentially annual_rain$n_rain
    # and seasonal_rain$n_rain
    count_names <- grepl(sum_rainday_col, data_by_year_names)
    
    if (!"TRUE" %in% count_names) { count_names <- grepl("sum_Rainday", data_by_year_names)}
    
    # so for sum_rainday columns - the cols counting the rainfall:
    if (sum(count_names) > 0){
      sum_rain_lists <- which(count_names == TRUE)
      for (i in sum_rain_lists){
        sum_rain_i <- data_by_year[[i]]
        
        # check if there is filtering - if it is filtered based on start and end rains then it is seasonal.
        # TODO: this isn't true. fix this. 
        if (any(grepl("filter", names(sum_rain_i)))){
          if (grepl("start_rain", sum_rain_i$filter_2) && grepl("end_", sum_rain_i$filter_2)){
            seasonal_count_raindays <- "TRUE"
            seasonal_sum_rain <- sum_rain_i
            # <=36 to encapsulate 365, 366
          } else if (grepl(">= 1", sum_rain_i$filter_2) && grepl("<= 36", sum_rain_i$filter_2)){
            annual_count_raindays <- "TRUE"
            annual_sum_rain <- sum_rain_i
          }
          # if there is no filtering, then it is annual.
        } else {
          annual_count_raindays <- "TRUE"
          annual_sum_rain <- sum_rain_i
        }
      }
    }
    
    # adding in the $ params for annual_rain ----------------------------------------
    if (as.logical(annual_total_rain) | as.logical(annual_count_raindays)){
      annual_rain_definition <- sum_rain_definitions(time = "annual_rain",
                                                     total_rain = annual_total_rain,
                                                     n_rain = annual_count_raindays,
                                                     sum_rain = annual_sum_rain,
                                                     n_raindays = annual_n_raindays,
                                                     data = NULL)
    } else {
      for (variable in variables_list) {
        annual_rain_definition[["annual_rain"]][[variable]] <- NA
      }
    }
    
    # adding in the $ params for seasonal_rain
    if (as.logical(seasonal_total_rain) | as.logical(seasonal_count_raindays)){
      seasonal_rain_definition <- sum_rain_definitions(time = "seasonal_rain", total_rain = seasonal_total_rain,
                                                       n_rain = seasonal_count_raindays, sum_rain = seasonal_sum_rain,
                                                       n_raindays = seasonal_n_raindays)#, data = data_definition)
    } else {
      for (variable in variables_list) {
        seasonal_rain_definition[["seasonal_rain"]][[variable]] <- NA
      }
    }
    
    # # todo: how to ensure we're getting the correct count.
    # if (annual_count_raindays){
    #   data_definition <- get_r_instat_definitions(data_book$get_calculations(data_name))
    # } else {
    #   if (seasonal_count_raindays){
    #     data_definition <- get_r_instat_definitions(data_book$get_calculations(data_name))
    #   } else {
    #     data_definition <- NULL
    #   }
    # }
  }
  
  rain_definitions <- c(annual_rain_definition, seasonal_rain_definition)
  return(rain_definitions)
}
