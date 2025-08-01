#' Extract Total Rainfall and Rain Day Definitions
#'
#' This function retrieves metadata definitions related to total rainfall and the number of rainy days
#' for both annual and seasonal periods. It attempts to identify the appropriate summary definitions
#' from a list of calculated values, checking for filters or column naming conventions that distinguish
#' between annual and seasonal data. It also supports extraction of the threshold used to define a "rain day"
#' based on raw definition metadata.
#'
#' @param data_by_year A list of summary definitions typically returned by `get_r_instat_definitions()` for the annual dataset.
#' @param annual_total_rain_col (Optional) Column name identifying the total annual rainfall.
#' @param seasonal_total_rain_col (Optional) Column name identifying the total seasonal rainfall.
#' @param annual_rainday_col (Optional) Column name identifying the total count of annual rain days.
#' @param seasonal_rainday_col (Optional) Column name identifying the total count of seasonal rain days.
#' @param definitions_in_raw (Optional) The raw metadata definitions (from unaggregated data) used to extract the rain day threshold.
#' @param rain_days_name (Optional) Name of the indicator (e.g., `"count"`) used to define a rain day threshold in the raw metadata.
#'
#' @return A named list containing:
#' \describe{
#'   \item{annual_rain}{A list of summary parameters for total rainfall and rain day count in the year.}
#'   \item{seasonal_rain}{A list of summary parameters for total rainfall and rain day count during the season.}
#' }
#' Each of these lists may include elements like `total_rain`, `n_rain`, `rain_day`, `na_rm`, `na_n`, etc.,
#' depending on what was found in the metadata. If definitions are missing or ambiguous, values will be set to `NA`.
#'
#' @details
#' The function looks for matches to known rainfall-related columns (e.g., `"sum_rain"`, `"n_rain"`) within the provided
#' list of definitions. Filtering logic is used to heuristically determine whether the definition applies to seasonal or annual data
#' based on filters such as `start_rain` and `end_` or numeric day-of-year ranges.
#'
#' If `definitions_in_raw` and `rain_days_name` are provided, the function will also extract the threshold used to define
#' a rain day (e.g., `>= 1mm`) from the raw metadata.
#'
#' @examples
#' \dontrun{
#' defs <- get_total_rain_counts(
#'   data_by_year = get_r_instat_definitions("ghana_by_station_year"),
#'   annual_total_rain_col = "sum_rain",
#'   annual_rainday_col = "count_rain",
#'   definitions_in_raw = get_r_instat_definitions("ghana"),
#'   rain_days_name = "count"
#' )
#' }
#'
#' @export
get_total_rain_counts <- function(data_by_year = NULL,
                                  annual_total_rain_col = NULL, seasonal_total_rain_col = NULL,
                                  annual_rainday_col = NULL, seasonal_rainday_col = NULL,
                                  definitions_in_raw = NULL, rain_days_name = NULL){
  
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
  annual_count_raindays <- "FALSE"
  annual_n_raindays <- NULL # always NULL?
  
  seasonal_total_rain <- "FALSE"
  seasonal_count_raindays <- "FALSE"
  seasonal_n_raindays <- NULL # always NULL?
  
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
    data_by_year_names <- names(data_by_year)
    
    #### TOTAL RAINFALL ########################################################
    # which of data_by_year are "sum_<rainfall>" -----------------------------
    # these are going to be potentially annual_rain$total_rain
    # and seasonal_rain$total_rain
    # and $na_rm $na_n $na_n_non $na_consec $na_prop
    total_rain_cols <- c(annual_total_rain_col, seasonal_total_rain_col)
    
    for (rain_col in total_rain_cols){
      count_names <- grepl(rain_col, data_by_year_names)
      
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
    }
    
    #### N RAINY DAYS ##########################################################
    # read in the name of the rainday count column for annual (and for seasonal)
    total_rainday_cols <- c(annual_rainday_col, seasonal_rainday_col)
    # these are going to be potentially annual_rain$n_rain
    # and seasonal_rain$n_rain
    
    # loop through those total_rainday_cols to find any
    for (rain_col in total_rainday_cols){
      count_names <- grepl(rain_col, data_by_year_names)
      
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
    }
    
    #### N RAINY DAYS ##########################################################
    # Then the rainy day threshold value (n_rain)
    # if we want to know the threshold for rainy days, then look in the raw data definitions (not the raw data)
    # rain_days_name = the name indicating if it is a rainy day or not in the raw data.
    # only runs if rain_days_name and definitions_in_raw (i.e., raw data) is given.
    annual_n_raindays <- get_rain_counts(definitions_in_raw, rain_days_name)$threshold
    
    # TODO: set up for seasonal
    ############################################################################
    
    
    # adding in the $ params for annual_rain ----------------------------------------
    if (any(as.logical(annual_total_rain)) || any(as.logical(annual_count_raindays))) {
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
    if (any(as.logical(seasonal_total_rain)) || any(as.logical(seasonal_count_raindays))) {
      seasonal_rain_definition <- sum_rain_definitions(time = "seasonal_rain",
                                                       total_rain = seasonal_total_rain,
                                                       n_rain = seasonal_count_raindays,
                                                       sum_rain = seasonal_sum_rain,
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
