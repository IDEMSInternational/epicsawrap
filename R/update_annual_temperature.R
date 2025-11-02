#' Update Annual Temperature Data
#'
#' @param data_frame A data frame containing the columns specified in `data_names`.
#' @param data_names A list of column names that are needed from the `data_frame`.
#' @param definitions A list containing definitions to be read in.
#' @param data_book The data book object where the data object is stored.
#'
#' @return A data frame in the data book at the year (and station) level containing annual temperature data
#' 
#' @export
update_annual_temperature <- function(data_frame, data_names, definitions, data_book){
    
    annual_temperature_definitions <- definitions$annual_temperature_summaries
    
    definitions_to_get <- names(annual_temperature_definitions)

    summaries <- c("min", "max", "mean")[c(
      any(grepl("^min_",  definitions_to_get)),
      any(grepl("^max_",  definitions_to_get)),
      any(grepl("^mean_", definitions_to_get))
    )]
      
    na_rm <- as_logical(annual_temperature_definitions$na_rm)
    na_prop <- as_numeric(annual_temperature_definitions$na_prop)
    na_n <- as_numeric(annual_temperature_definitions$na_n)
    na_consec <- as_numeric(annual_temperature_definitions$na_consec)
    na_n_non <- as_numeric(annual_temperature_definitions$na_n_non)
    to = annual_temperature_definitions$to
    
    summary_temperature <- rpicsa::summary_temperature(data = data_frame,
                                                       date_time = data_names$date,
                                                       tmin = data_names$tmin,
                                                       tmax = data_names$tmax,
                                                       year = data_names$year,
                                                       month = data_names$month,
                                                       station = data_names$station,
                                                       to = to,
                                                       summaries = summaries,
                                                       na_rm = na_rm,
                                                       na_prop = na_prop,
                                                       na_n = na_n,
                                                       na_consec = na_consec,
                                                       na_n_non = na_n_non,
                                                       data_book = data_book
                                                  )
}
