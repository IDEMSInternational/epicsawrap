#' Update Start Rains
#'
#' @param data_frame A data frame containing the columns specified in `data_names`.
#' @param data_names A list of column names that are needed from the `data_frame`.
#' @param definitions A list containing definitions to be read in.
#' @param data_book The data book object where the data object is stored.
#'
#' @return A data frame in the data book at the year (and station) level containing end of season data.
#' @export
update_end_season <- function(data_frame, data_names, definitions, data_book){
  
    end_season_definitions <- definitions$annual_summaries$end_season
    start_day <- as_numeric(end_season_definitions$start_day)
    if (is.null(start_day)) start_day <- 1
    end_day <- as_numeric(end_season_definitions$end_day)
    if (is.null(end_day)) end_day <- 366
    capacity <- as_numeric(end_season_definitions$capacity)
    water_balance_max <- as_numeric(end_season_definitions$water_balance_max)
    evaporation <- end_season_definitions$evaporation
    evaporation_value <- as_numeric(end_season_definitions$evaporation_value)
    output <- end_season_definitions$output
    if (is.null(output) || output == "both") output <- c("doy", "date", "status")
    s_start_doy <- as.numeric(end_season_definitions$s_start_doy)
    s_start_month <- as.numeric(format(as.Date(s_start_doy, origin = "2020-12-31"), "%m"))   # picking a non-leap-year
    drop <- as_logical(end_season_definitions$drop)
    if (length(drop) == 0) drop <- FALSE
    reducing <- as_logical(end_season_definitions$reducing)
    reducing_value <- as_numeric(end_season_definitions$reducing_value)
    evaporation_variable <- end_season_definitions$evaporation_variable
    
    end_season <- rpicsa::end_season(data=data_frame,
                                     date_time=data_names$date,
                                     station=data_names$station,
                                     year=data_names$year,
                                     rain=data_names$rain,
                                     doy=data_names$doy,
                                     s_start_month = s_start_month,
                                     drop = drop,
                                     start_day = start_day,
                                     end_day = end_day,
                                     output = output,
                                     capacity = capacity,
                                     water_balance_max = water_balance_max,
                                     evaporation = evaporation,
                                     evaporation_value = evaporation_value,
                                     evaporation_variable = evaporation_variable,
                                     reducing = FALSE,
                                     reducing_value = reducing_value,
                                     data_book = data_book)
}
