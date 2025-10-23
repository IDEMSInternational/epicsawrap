#' Update Start Rains
#'
#' @param data_frame A data frame containing the columns specified in `data_names`.
#' @param data_names A list of column names that are needed from the `data_frame`.
#' @param definitions A list containing definitions to be read in.
#' @param data_book The data book object where the data object is stored.
#' 
#' @return A data frame in the data book at the year (and station) level containing start of rains data.
#' @export
update_start_rains <- function(data_frame, data_names, definitions, data_book){
  
  start_rains_definitions <- definitions$annual_summaries$start_rains
  
  threshold <- as_numeric(start_rains_definitions$threshold)
  start_day <- as_numeric(start_rains_definitions$start_day)
  end_day <- as_numeric(start_rains_definitions$end_day)
  total_rainfall_over_days <- as_numeric(start_rains_definitions$over_days)
  amount_rain <- as_numeric(start_rains_definitions$amount_rain)
  prob_rain_day <- as_numeric(start_rains_definitions$prob_rain_day)
  
  proportion <- as_logical(start_rains_definitions$proportion)
  evaporation <- as_logical(start_rains_definitions$evaporation)
  if (proportion) total_rainfall_comparison <- "proportion"
  else if (evaporation) total_rainfall_comparison <- "evaporation"
  else total_rainfall_comparison <- "amount"
  # TODO: what about the "evaporation" option? Need to add in evaporation TRUE/FALSE into the definitions file. 
  evaporation_variable <- start_rains_definitions$evaporation_variable
  fraction <- as_numeric(start_rains_definitions$fraction)
  
  dry_spell <- as_logical(start_rains_definitions$dry_spell)
  spell_interval <- as_numeric(start_rains_definitions$spell_interval)
  spell_max_dry_days <- as_numeric(start_rains_definitions$spell_max_dry_days)
  
  dry_period <- as_logical(start_rains_definitions$dry_period)
  period_interval <- as.numeric(start_rains_definitions$period_interval)
  max_rain <- as.numeric(start_rains_definitions$max_rain)
  period_max_dry_days <- as.numeric(start_rains_definitions$period_max_dry_days)
  
  # TODO: is number of rainy days being read in?
  number_rain_days = as_logical(start_rains_definitions$number_rain_days)
  min_rain_days = as_numeric(start_rains_definitions$min_rain_days)
  rain_day_interval = as_numeric(start_rains_definitions$rain_day_interval)
  
  output <- start_rains_definitions$output
  if (is.null(output) || output == "both") output <- c("doy", "date", "status")
  s_start_doy <- as.numeric(start_rains_definitions$s_start_doy)
  s_start_month <- as.numeric(format(as.Date(s_start_doy, origin = "2020-12-31"), "%m"))   # picking a non-leap-year
  drop <- as_logical(start_rains_definitions$drop)
  if (length(drop) == 0) drop <- FALSE
  
  start_rains <- rpicsa::start_rains(data=data_frame,
                                     date_time=data_names$date,
                                     station=data_names$station,
                                     year=data_names$year,
                                     rain=data_names$rain,
                                     doy=data_names$doy,
                                     threshold=threshold,
                                     start_day=start_day,
                                     end_day=end_day,
                                     s_start_month = s_start_month,
                                     drop = drop,
                                     output = output,
                                     total_rainfall_over_days = total_rainfall_over_days,
                                     total_rainfall_comparison = "amount", # No value in defintions file. Default used.
                                     amount_rain = amount_rain,
                                     prob_rain_day = prob_rain_day,
                                     evaporation_variable = NULL, # No value in defintions file. Default used.
                                     fraction = 0.5, # No value in defintions file. Default used.
                                     number_rain_days = FALSE, # No value in defintions file. Default used.
                                     min_rain_days = 1, # No value in defintions file. Default used.
                                     rain_day_interval = 2, # No value in defintions file. Default used.
                                     dry_spell = dry_spell,
                                     spell_interval = spell_interval,
                                     spell_max_dry_days = spell_max_dry_days,
                                     dry_period = dry_period,
                                     period_interval = period_interval,
                                     max_rain = max_rain,
                                     period_max_dry_days = period_max_dry_days,
                                     data_book = data_book)
}