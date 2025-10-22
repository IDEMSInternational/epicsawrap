#' Update Start Rains
#'
#' @param data_frame A data frame containing the columns specified in `data_names`.
#' @param data_names A list of column names that are needed from the `data_frame`.
#' @param definitions A list containing definitions to be read in.
#' @param data_book The data book object where the data object is stored.
#'
#' @export
#'
#' @examples
#' library(rpicsa)
#' library(databook)
#'
#' # 1. Let's set up our data book
#' data_book <- DataBook$new()
#'
#' # 2. Importing in some data for testing (this is stored in the rpicsa package)
#' data(daily_niger)
#' data_book$import_data(list(daily_niger = daily_niger))
#' 
#' # 3. Read in our definitions data
#' definitions <- jsonlite::fromJSON("C:/Users/HP/Downloads/test_json_1.json")
#'
#' # 4. Put in "data_names" the names of all the variables we're going to use from the daily_niger data. 
#' # Looking at our rpicsa::annual_rain, this can be
#' # station, year, and rain
#' data_names <- list(date = "date", 
#'                    rain = "rain",
#'                    year = "year", 
#'                    doy = "doy",
#'                    station = "station_name")
#' 
#' 
#' \dontrun{
#'   update_end_season(data_frame = "daily_niger", 
#'                      data_names = data_names, 
#'                      definitions = definitions, 
#'                      data_book = data_book)
#' }
#' 
update_end_season <- function(data_frame, data_names, definitions, data_book){
  
    end_season_definitions <- definitions$annual_summaries$end_season
    
    start_day <- as.numeric(end_season_definitions$start_day)
    end_day <- as.numeric(end_season_definitions$end_day)
    capacity <- as.numeric(end_season_definitions$capacity)
    water_balance_max <- as.numeric(end_season_definitions$water_balance_max)
    evaporation <- as.character(end_season_definitions$evaporation)
    evaporation_value <- as.numeric(end_season_definitions$evaporation_value)
    
    end_season <- rpicsa::end_season(data=data_frame,
                                     date_time=data_names$date,
                                     station=data_names$station,
                                     year=data_names$year,
                                     rain=data_names$rain,
                                     doy=data_names$doy,
                                     s_start_month = 1, # No value in defintions file. Default used.
                                     drop = TRUE, # No value in defintions file. Default used.
                                     start_day = start_day,
                                     end_day = end_day,
                                     output = c("doy", "date", "status"), # No value in defintions file. Default used.
                                     capacity = capacity,
                                     water_balance_max = water_balance_max,
                                     evaporation = evaporation,
                                     evaporation_value = evaporation_value,
                                     evaporation_variable = NULL, # No value in defintions file. Default used.
                                     reducing = FALSE, # No value in defintions file. Default used.
                                     reducing_value = 0.5, # No value in defintions file. Default used.
                                     data_book = data_book)
}