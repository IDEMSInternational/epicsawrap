#' Update End of Rains Definition 
#' 
#' @description Runs the `end_rains` function from the `rpicsa` package by reading in the parameter values from the
#' definitions 
#'
#' @param data_frame A data frame containing the columns specified in `data_names`.
#' @param data_names A list of column names that are needed from the dataframe.
#' @param definitions A list containing definitions to be read in.
#' @param data_book The data book object where the data object is stored.
#'
#' @return TODO
#' @export
#' 
update_end_rains <- function(data_frame, data_names, definitions, data_book){
  end_rains_definitions <- definitions$annual_summaries$end_rains
  
  start_day <- as.numeric(end_rains_definitions$start_day)
  end_day <- as.numeric(end_rains_definitions$end_day)
  # for the output, when I checked, the value in the definitions was `output="both"`, but we currently
  # have "doy", "date" or "status" as the options.
  output <- as.character(end_rains_definitions$output)
  if (output == "both") output <- c("doy", "date", "status")
  min_rainfall <- as.numeric(end_rains_definitions$min_rainfall)
  interval_length <- as.numeric(end_rains_definitions$interval_length)
  s_start_doy <- as.numeric(end_rains_definitions$s_start_doy)
  s_start_month <- as.numeric(format(as.Date(s_start_doy, origin = "2020-12-31"), "%m"))   # picking a non-leap-year
  drop <- as.logical(end_rains_definitions$drop)
  if (length(drop) == 0 | is.null(drop)) drop <- FALSE
  
  rpicsa::end_rains(data = "daily_niger",
                    date_time = data_names$date,
                    station = data_names$station,
                    year = data_names$year,
                    rain = data_names$rain,
                    doy = data_names$doy,
                    s_start_month = s_start_month,
                    drop = drop,
                    start_day = start_day,
                    end_day = end_day,
                    output = output,
                    interval_length = interval_length,
                    min_rainfall = min_rainfall,
                    data_book = data_book)
}