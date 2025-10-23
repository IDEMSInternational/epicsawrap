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


#' Update Monthly Temperature Data
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
#'                    tmin = "tmin",
#'                    tmax = "tmax",
#'                    year = "year", 
#'                    month = "month",
#'                    station = "station_name")
#' 
#' 
#' \dontrun{
#'   update_monthly_temperature(data_frame = "daily_niger", 
#'                             data_names = data_names, 
#'                             definitions = definitions, 
#'                             data_book = data_book)
#' }
#' 
update_monthly_temperature <- function(data_frame, data_names, definitions, data_book){
  
    monthly_temperature_definitions <- definitions$monthly_temperature_summaries$min_tmin
    
    na_rm <- as_logical(monthly_temperature_definitions$na_rm)
    na_prop <- as_numeric(monthly_temperature_definitions$na_prop)
    na_n <- as_numeric(monthly_temperature_definitions$na_n)
    na_consec <- as_numeric(monthly_temperature_definitions$na_consec)
    na_n_non <- as_numeric(monthly_temperature_definitions$na_n_non)
    to = monthly_temperature_definitions$to
    
    
    summary_temperature <- rpicsa::summary_temperature(data=data_frame,
                                                       date_time=data_names$date,
                                                       tmin=data_names$tmin,
                                                       tmax=data_names$tmax,
                                                       year=data_names$year,
                                                       month=data_names$month,
                                                       station=data_names$station,
                                                       to=to,
                                                       summaries=c("mean", "min", "max"),
                                                       na_rm=na_rm,
                                                       na_prop=na_prop,
                                                       na_n=na_n,
                                                       na_consec=na_consec,
                                                       na_n_non=na_n_non,
                                                       data_book=data_book
                                                      )
}