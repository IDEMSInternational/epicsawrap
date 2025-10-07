#' Update End of rains 
#' @description Runs the `end_rains` function from the `rpicsa` package by reading in the parameter values from the
#' definitions 
#'
#' @param data_frame A data frame containing the columns specified in `data_names`.
#'
#' @param data_names A list of column names that are needed from the dataframe.
#'
#' @param definitions A list containing definitions to be read in.
#'
#' @param data_book The data book object where the data object is stored.
#'
#' @export
#' 
#' @examples
#' 
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
#'                    station = "station_name", 
#'                    year = "year", 
#'                    rain = "rain", 
#'                    doy = "doy")
#' 
#' 
#' \dontrun{
#'   update_end_rains(data_frame = "daily_niger", 
#'                    data_names = data_names, 
#'                    definitions = definitions, 
#'                    data_book = data_book)
#' }
#'
update_end_rains <- function(data_frame, data_names, definitions, data_book){
  end_rains_definitions <- definitions$annual_summaries$end_rains
  
  start_day <- as.numeric(end_rains_definitions$start_day)
  end_day <- as.numeric(end_rains_definitions$end_day)
  # for the output, when I checked, the value in the definitions was `output="both"`, but we currently
  # have "doy", "date" or "status" as the options.
  output <- as.character(end_rains_definitions$output) 
  min_rainfall <- as.numeric(end_rains_definitions$min_rainfall)
  interval_length <- as.numeric(end_rains_definitions$interval_length)
  
  rpicsa::end_rains(data = data_frame,
                    date_time = data_names$date,
                    station = data_names$station,
                    year = data_names$year,
                    rain = data_names$rain,
                    doy = data_names$doy,
                    s_start_month = 1, # this parameter was not in the definitions
                    drop = TRUE, # this was also not in the definitions
                    start_day = start_day,
                    end_day = end_day,
                    output = output,
                    interval_length = interval_length,
                    min_rainfall = min_rainfall,
                    data_book = data_book)
}