#' Safely convert an Object to a Logical value
#'
#' @param x An object to be converted to a logical value If `x` is `NULL`, the function returns `NULL` instead of an error.
#'
#' @return
#' A logical value if conversion is successful, or `NULL` if the input is `NULL`.
as_logical <- function(x) {
  if (is.null(x)) {
    return(FALSE)
  }
  as.logical(x)
}

#' Safely convert an Object to Numeric
#'
#' @param x An object to be converted to numeric. If `x` is `NULL`, the function returns `NULL` instead of an error.
#'
#' @return
#' A numeric value if conversion is successful, or `NULL` if the input is `NULL`.
as_numeric <- function(x){
  if (is.null(x)){
      return(NULL)
  }
  as.numeric(x)
}


#' Update Seasonal Rain Data
#'
#' @param data_frame A data frame containing the columns specified in `data_names`.
#' @param data_names A list of column names that are needed from the `data_frame`.
#' @param summary_data_frame A summary data frame containing the columns specified in `summary_data_names`.
#' @param summary_data_names A list of column names that are needed from the `summary_data_frame`.
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
#'                    station = "station_name", 
#'                    year = "year", 
#'                    rain = "rain", 
#'                    doy = "doy")
#'                    
#' # 5. Defining the "summary_data_names" list
#' summary_data_names <- list(start_date = NULL, 
#'                            end_date = NULL)              
#' 
#' 
#' \dontrun{
#'   update_seasonal_rain(data_frame = "daily_niger", 
#'                        data_names = data_names, 
#'                        summary_data_frame = NULL,
#'                        summary_data_names = summary_data_names,
#'                        definitions = definitions, 
#'                        data_book = data_book)
#' }
#' 
update_seasonal_rain <- function(data_frame, data_names, summary_data_frame, summary_data_names,
                                 definitions, data_book){
  seasonal_rain_definitions <- definitions$annual_summaries$seasonal_rain
  
  total_rain <- as_logical(seasonal_rain_definitions$total_rain)
  n_rain <- as_logical(seasonal_rain_definitions$n_rain)
  na_rm <- as_logical(seasonal_rain_definitions$na_rm)
  rain_day <- as.numeric(seasonal_rain_definitions$rain_day) 
  na_n <- as_numeric(seasonal_rain_definitions$na_n)
  na_n_non <- as_numeric(seasonal_rain_definitions$na_n_non)
  na_consec <- as_numeric(seasonal_rain_definitions$na_consec)
  na_prop <- as_numeric(seasonal_rain_definitions$na_prop)
  
  seasonal_rain(summary_data = summary_data_frame, 
                start_date = summary_data_names$start_date, 
                end_date = summary_data_names$end_date, 
                data = data_frame, 
                date_time = data_names$date, 
                year = data_names$year, 
                station = data_names$station, 
                doy = data_names$doy, 
                rain = data_names$rain, 
                s_start_month = 1, # I used the default value because it wasn't in the definitions
                total_rain = total_rain, 
                n_rain = n_rain, 
                rain_day = rain_day, 
                na_rm = na_rm, 
                na_prop = na_prop, 
                na_n = na_n, 
                na_consec = na_consec, 
                na_n_non = na_n_non, 
                data_book = data_book)
}
