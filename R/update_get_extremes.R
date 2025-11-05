#' Update Get Extremes 
#'
#' @param data_frame A data frame containing the columns specified in `data_names`.
#' @param data_names A list of column names that are needed from the `data_frame`.
#' @param definitions A list containing definitions to be read in.
#' @param data_book The data book object where the data object is stored.
#' @param element Determines the specific variant of the definitions to use. 
#'
#' @return A data frame in the data book at the year (and station) level containing extremes data.
#' @export
#'
update_get_extremes <- function(data_frame, data_names, definitions, data_book,
                                element = c("extreme_rain", "extreme_tmin", "extreme_tmax")){
    element <- match.arg(element)
    get_extremes_definitions <- switch(
      element,
      extreme_rain = definitions$annual_summaries$extreme_rain,
      extreme_tmin = definitions$annual_summaries$extreme_tmin,
      extreme_tmax = definitions$annual_summaries$extreme_tmax,
      stop("Unknown extreme definition: ", element)
    )
    
    direction <- get_extremes_definitions$direction
    lb_value <- as.numeric(get_extremes_definitions$value_lb)
    value <- as.numeric(get_extremes_definitions$value)
    na_rm <- as_logical(get_extremes_definitions$na_rm)
    na_n <- as_numeric(get_extremes_definitions$na_n)
    na_n_non <- as_numeric(get_extremes_definitions$na_n_non)
    na_consec <- as_numeric(get_extremes_definitions$na_consec)
    na_prop <- as_numeric(get_extremes_definitions$na_prop)
    
    get_extremes <- rpicsa::get_extremes(data = data_frame,
                                         element = data_names$element, 
                                         date_time = data_names$date,
                                         year = data_names$year,
                                         station = data_names$station,
                                         direction = direction,
                                         s_start_month = 1, # No value in definitions. Used default.
                                         value = value,
                                         lb_value = lb_value, 
                                         na_rm = na_rm, 
                                         na_prop = na_prop, 
                                         na_n = na_n, 
                                         na_consec = na_consec, 
                                         na_n_non = na_n_non, 
                                         data_book = data_book)
    
}