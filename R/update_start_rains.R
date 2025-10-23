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
    prob_rain_day <- as.numeric(start_rains_definitions$prob_rain_day)
    dry_spell <- as_logical(start_rains_definitions$dry_spell)
    spell_interval <- as_numeric(start_rains_definitions$spell_interval)
    spell_max_dry_days <- as_numeric(start_rains_definitions$spell_max_dry_days)
    dry_period <- as_logical(start_rains_definitions$dry_period)
    period_interval <- as.numeric(start_rains_definitions$period_interval)
    max_rain <- as.numeric(start_rains_definitions$max_rain)
    period_max_dry_days <- as.numeric(start_rains_definitions$period_max_dry_days)
    
    start_rains <- rpicsa::start_rains(data=data_frame,
                                       date_time=data_names$date,
                                       station=data_names$station,
                                       year=data_names$year,
                                       rain=data_names$rain,
                                       doy=data_names$doy,
                                       threshold=threshold,
                                       start_day=start_day,
                                       end_day=end_day,
                                       s_start_month = 1, # No value in defintions file. Default used.
                                       drop = TRUE, # No value in defintions file. Default used.
                                       output = c("doy", "date", "status"), # No value in defintions file. Default used.
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