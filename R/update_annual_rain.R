#' Update Rain Rain Data
#'
#' @param data_frame A data frame containing the columns specified in `data_names`.
#' @param data_names A list of column names that are needed from the `data_frame`.
#' @param definitions A list containing definitions to be read in.
#' @param data_book The data book object where the data object is stored.
#' 
#' @return A data frame in the data book at the year (and station) level containing annual rainfall data.
#' 
#' @export

update_annual_rain <- function(data_frame, data_names, definitions, data_book){
  
  annual_rain_definitions <- definitions$annual_rain
  if (is.null(annual_rain_definitions)){
    annual_rain_definitions <- definitions$annual_summaries$annual_rain
  }
  
  total_rain <- as_logical(annual_rain_definitions$total_rain)
  n_rain <- as_logical(annual_rain_definitions$n_rain)
  rain_day <- as.numeric(annual_rain_definitions$rain_day)
  na_rm <- as_logical(annual_rain_definitions$na_rm)
  na_n <- as_numeric(annual_rain_definitions$na_n)
  na_prop <- as_numeric(annual_rain_definitions$na_prop)
  na_consec <- as_numeric(annual_rain_definitions$na_consec)
  na_n_non <- as_numeric(annual_rain_definitions$na_n_non)
  
  annual_rain <- rpicsa::annual_rain(
    data = data_frame, 
    year = data_names$year, 
    station = data_names$station, 
    rain = data_names$rain, 
    total_rain = total_rain,
    n_rain = n_rain,
    rain_day = rain_day,
    na_rm = na_rm,
    na_prop = na_prop,
    na_n = na_n,
    na_consec = na_consec,
    na_n_non = na_n_non,
    data_book = data_book
  )
}