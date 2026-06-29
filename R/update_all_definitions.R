
#' Update All Definitions
#'
#' @description
#' This function updates the data for all the definitions
#'
#' @param data_frame A data frame containing the main dataset whose definitions are to be updated.
#' @param data_names A list of column names that are needed from  `data_frame`.
#' @param summary_data_frame A data frame containing summary data corresponding to the main dataset.
#' @param summary_data_names A list of column names that are needed from `summary_data_frame`.
#' @param definitions A list containing metadata or variable definitions to be updated.
#' @param data_book The data book object where the data object is stored.
#'
#' @return
#' The updated data for all of the definition files.
#'
#' @export
#'
update_all_definitions <- function(data_frame, data_names, summary_data_frame, summary_data_names, definitions, 
                                   data_book){
    if (is.list(data_names)){
        updated_annual_temperature <- update_annual_temperature(data_frame = data_frame, data_names = data_names,
                                                                definitions = definitions, data_book = data_book)
        
        updated_end_rains <- update_end_rains(data_frame = data_frame, data_names = data_names,
                                              definitions = definitions, data_book = data_book)
        
        updated_end_season <- update_end_season(data_frame = data_frame, data_names = data_names,
                                                definitions = definitions, data_book = data_book)
        
        updated_monthly_temperature <- update_monthly_temperature(data_frame = data_frame, definitions = definitions,
                                                                  data_names = data_names, data_book = data_book)
        
        updated_seasonal_rain <- update_seasonal_rain(data_frame = data_frame, data_names = data_names,
                                                      summary_data_frame = summary_data_frame, 
                                                      summary_data_names = summary_data_names,
                                                      definitions = definitions, data_book = data_book)
        
        updated_start_rains <- update_start_rains(data_frame = data_frame, data_names = data_names, 
                                                  definitions = definitions, data_book = data_book)
        
        # TO DO: Once the following functions are merged, call them here
        # update_annual_rain
        # update_get_extremes
        # update_crop_definitions
    }
    
}