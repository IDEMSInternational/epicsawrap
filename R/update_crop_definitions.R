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
update_crops_definitions <- function(data_frame, data_names, seasonal_data_frame, seasonal_data_names, definitions, data_book){
    actual_crop_definitions <- definitions$crops_success
    
    rain_totals <- as.numeric(actual_crop_definitions$water_requirements)
    plant_dates_from <- as.numeric(actual_crop_definitions$planting_dates$from)
    plant_dates_to <- as.numeric(actual_crop_definitions$planting_dates$to)
    plant_dates_by <- as.numeric(actual_crop_definitions$planting_dates$by)
    plant_days <- c(plant_dates_from, plant_dates_to, plant_dates_by)
    plant_lengths <- as.numeric(actual_crop_definitions$planting_length)
    
    crop_definitions <- crops_definitions(data_name = data_frame,
                                          date_time = data_names$date,
                                          year = data_names$year,
                                          station = data_names$station,
                                          doy = data_names$doy,
                                          rain = data_names$rain,
                                          s_start_month = 1, # No value in definitions. Used default.
                                          rain_totals = rain_totals,
                                          plant_days = plant_days,
                                          plant_lengths = plant_lengths, # In the rpicsa::crops_definitions help, it says this should be a sequence of three numbers, but in the definitions file, it's just two numbers. 
                                          start_check = c("both", "yes", "no"), # No value in definitions. Used default.
                                          season_data_name = seasonal_data_frame, # I believe this should potentially be a second dataframe that has `start_day` and `end_day` columns, hence I added it as a parameter to the function.
                                          start_day = seasonal_data_names$start_day,
                                          end_day = seasonal_data_names$start_day,
                                          return_crops_table = TRUE, # No value in definitions. Used default.
                                          definition_props = TRUE, # No value in definitions. Used default.
                                          data_book = data_book
                                      )
}