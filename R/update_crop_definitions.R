#' @title Get Values by Sequence or List
#'
#' This function returns a sequence of values from the specified property in the `definitions`
#'
#' @param definitions A list containing data or metadata properties from which to extract values.
#' @param property A character string giving the name of the property to retrieve from the definitions object.
#'
#' @return
#' A vector (sequence) of values corresponding to the requested property.
get_values_by_sequence_or_list <- function(definitions, property){
    if (is.list(definitions)){
        property_from <- as.numeric(definitions[[property]][["from"]])
        property_to <- as.numeric(definitions[[property]][["to"]])
        property_by <- as.numeric(definitions[[property]][["by"]])
        property_results <- c(property_from, property_to, property_by)
    }
    else {
        property_results <- as.numeric(definitions$property)
    }
    return(property_results)
}

#' Update Crops Definition 
#'
#' @param data_frame A data frame containing the columns specified in `data_names`.
#' @param data_names A list of column names that are needed from the dataframe.
#' @param seasonal_data_frame A data frame containing the columns specified in `seasonal_data_names`.
#' @param seasonal_data_names A list of column names that are needed from the `seasonal_data_frame`.
#' @param definitions A list containing definitions to be read in.
#' @param data_book The data book object where the data object is stored.
#'
#' @return A data frame in the data book at the year (and station) level containing crops definitions data.
#' @export
#' 
#' 
#'
update_crops_definitions <- function(data_frame, data_names, seasonal_data_frame, seasonal_data_names, definitions, data_book){
    actual_crop_definitions <- definitions$crops_success
    season_start_probs_definitions <- definitions$season_start_probabilities
    
    # crop_success_props
    s_start_month <- actual_crop_definitions$s_start_month
    start_check <- actual_crop_definitions$start_check
    return_crops_table <- actual_crop_definitions$return_crops_table
    rain_totals <- get_values_by_sequence_or_list(actual_crop_definitions, "water_requirements")
    plant_days <- get_values_by_sequence_or_list(actual_crop_definitions, "planting_dates")
    plant_lengths <- get_values_by_sequence_or_list(actual_crop_definitions, "planting_length")
    
    # season_start_probabilities props
    season_s_start_month <- season_start_probs_definitions$s_start_month
    season_start_check <- season_start_probs_definitions$start_check
    season_definition_props <- season_start_probs_definitions$definition_props
    season_planting_days <- get_values_by_sequence_or_list(season_start_probs_definitions, "planting_days")
    season_rain_totals <- get_values_by_sequence_or_list(season_start_probs_definitions, "rain_totals")
    specified_day <- get_values_by_sequence_or_list(season_start_probs_definitions, "specified_day")
  
    if (is.null(season_start_probs_definitions)){
        definition_props = FALSE
    }
    if (is.null(actual_crop_definitions)){
        return_crops_table = FALSE
    }
    
    if (is.null(season_start_probs_definitions) & !is.null(actual_crop_definitions)){
      crop_definitions <- crops_definitions(data_name = data_frame, date_time = data_names$date, year = data_names$year, 
                                            station = data_names$station, doy = data_names$doy, rain = data_names$rain,
                                            s_start_month = s_start_month, rain_totals = rain_totals, plant_days = plant_days,
                                            plant_lengths = plant_lengths,  start_check = start_check, season_data_name = seasonal_data_frame, 
                                            start_day = seasonal_data_names$start_day, end_day = seasonal_data_names$start_day,
                                            return_crops_table = return_crops_table, definition_props = definition_props, data_book = data_book)
      
    }
    else if (!is.null(season_start_probs_definitions) & is.null(actual_crop_definitions)){
      # the planting_days and planting_lengths are not being read i because they're not in the definitions file
      # This might cause errors as those parameters have no default values.
      crop_definitions <- crops_definitions(data_name = data_frame, date_time = data_names$date, year = data_names$year, 
                                            station = data_names$station, doy = data_names$doy, rain = data_names$rain,
                                            s_start_month = season_s_start_month, plant_lengths = specified_day, 
                                            rain_totals =  season_rain_totals, plant_days = season_planting_days,  
                                            start_check = season_start_check, season_data_name = seasonal_data_frame, 
                                            start_day = seasonal_data_names$start_day, end_day = seasonal_data_names$start_day, 
                                            return_crops_table = return_crops_table, definition_props = season_definition_props, 
                                            data_book = data_book)
    }
    else if (!is.null(season_start_probs_definitions) & !is.null(actual_crop_definitions)){
        if (identical(rain_totals, season_rain_totals) & identical(plant_days, season_planting_days) &
            identical(plant_lengths, specified_day)){
            crop_definitions <- crops_definitions(data_name = data_frame, date_time = data_names$date, year = data_names$year, 
                                                  station = data_names$station, doy = data_names$doy, rain = data_names$rain,
                                                  s_start_month = s_start_month, rain_totals = rain_totals, plant_days = plant_days,
                                                  plant_lengths = plant_lengths,  start_check = start_check, season_data_name = seasonal_data_frame, 
                                                  start_day = seasonal_data_names$start_day, end_day = seasonal_data_names$start_day,
                                                  return_crops_table = return_crops_table, definition_props = TRUE, data_book = data_book)
        }
        else {
            warning("Season Start Probabilities Definition differs from Crop Success")
            # crop success
            crop_definitions <- crops_definitions(data_name = data_frame, date_time = data_names$date, year = data_names$year, 
                                                  station = data_names$station, doy = data_names$doy, rain = data_names$rain,
                                                  s_start_month = s_start_month, rain_totals = rain_totals, plant_days = plant_days,
                                                  plant_lengths = plant_lengths,  start_check = start_check, season_data_name = seasonal_data_frame, 
                                                  start_day = seasonal_data_names$start_day, end_day = seasonal_data_names$start_day,
                                                  return_crops_table = return_crops_table, definition_props = TRUE, data_book = data_book)
            # season_start_probabilities
            season_crop_definitions <- crops_definitions(data_name = data_frame, date_time = data_names$date, year = data_names$year, 
                                                         station = data_names$station, doy = data_names$doy, rain = data_names$rain,
                                                         s_start_month = season_s_start_month, rain_totals =  season_rain_totals, 
                                                         plant_days = season_planting_days, plant_lengths = specified_day,  start_check = season_start_check, 
                                                         season_data_name = seasonal_data_frame, start_day = seasonal_data_names$start_day, 
                                                         end_day = seasonal_data_names$start_day, return_crops_table = return_crops_table, 
                                                         definition_props = season_definition_props, data_book = data_book)
        }
      
    }
}