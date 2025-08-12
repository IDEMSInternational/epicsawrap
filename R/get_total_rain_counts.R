#' Extract Annual and Seasonal Rainfall Definitions
#'
#' This function retrieves metadata definitions for total rainfall and the number of rainy days, 
#' separately for annual and seasonal periods. It searches the supplied summary definitions for 
#' rainfall-related indicators, using naming conventions and filter criteria to distinguish 
#' annual from seasonal data. Optionally, it also extracts the rain-day threshold (e.g., `>= 1 mm`) 
#' from raw metadata.
#' 
#' @param data_by_year A named list of summary definitions, typically returned by 
#'   [get_r_instat_definitions()], for the annual dataset.
#' @param annual_total_rain_col (Optional) Column name for total annual rainfall.
#' @param seasonal_total_rain_col (Optional) Column name for total seasonal rainfall.
#' @param annual_rainday_col (Optional) Column name for the annual rain-day count.
#' @param seasonal_rainday_col (Optional) Column name for the seasonal rain-day count.
#' @param definitions_in_raw (Optional) Raw metadata definitions (from unaggregated data), 
#'   used to extract the rain-day threshold.
#' @param rain_days_name (Optional) Name of the indicator in the raw metadata (e.g., `"count"`) 
#'   used to define a rain-day threshold.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{annual_rain}{List of summary parameters for annual rainfall totals and rain-day counts.}
#'   \item{seasonal_rain}{List of summary parameters for seasonal rainfall totals and rain-day counts.}
#' }
#' Each of these lists may include:
#' \itemize{
#'   \item `total_rain` – indicator for total rainfall.
#'   \item `n_rain` – indicator for number of rain days.
#'   \item `rain_day` – rain-day threshold definition.
#'   \item `na_rm`, `na_n`, `na_n_non`, `na_consec`, `na_prop` – metadata about missing-value handling.
#' }
#' If a definition cannot be found or is ambiguous, the corresponding values will be set to `NA`.
#'
#' @details
#' The function attempts to match rainfall indicators using the column names provided 
#' (e.g., `"sum_rain"`, `"count_rain"`).  
#' Heuristics are then applied to decide if a match refers to annual or seasonal data:
#' \itemize{
#'   \item Definitions filtered by variables like `start_rain` and `end_` are considered seasonal.
#'   \item Definitions without such filters are assumed to be annual.
#' }
#' If `definitions_in_raw` and `rain_days_name` are provided, the function calls 
#' [get_rain_counts()] on the raw metadata to retrieve the rain-day threshold.
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
