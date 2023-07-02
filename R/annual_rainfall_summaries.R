#' Annual Rainfall Summaries
#' 
#' @description A table containing all the annual rainfall summaries for PICSA
#' e.g. start of rain, total rainfall, number of rain days, end of season.
#' One row per year/station and one column per summary.
#'
#' @param country `character(1)` The country code of the data.
#' @param station_id `character` The id's of the stations to analyse. Either a
#'   single value or a vector.
#' @param summaries `character` The names of the summaries to produce.
#'
#' @return A data frame with yearly summaries.
#' @export
#' @importFrom rlang :=
#' @examples
#' #annual_rainfall_summaries(country = "zm", station_id = "01122", summaries = "annual_rain")
#' #annual_rainfall_summaries(country = "zm", station_id = "16", 
#' #                          summaries = c("start_rains", "end_rains", "annual_rain", "seasonal_rain")) #, "end_season"))
annual_rainfall_summaries <- function(country,
                                      station_id, 
                                      # for now, just one summary: annual_rain
                                      summaries = c("annual_rain", # total rain days and annual rainfall
                                                    "start_rains",
                                                    "end_rains",
                                                    "end_season",
                                                    "seasonal_rain",
                                                    "seasonal_length")
) {
  
  # checks for start_rains and end_rains for seasonal_length and seasonal_rain
  if (any(grepl("seasonal_", summaries)) & (!"start_rains" %in% summaries)){ stop("start_rains required to calculate seasonal_rain and seasonal_length") }
  if (any(grepl("seasonal_", summaries)) & (any(grepl("end_", summaries)) == FALSE)){ stop("end_rains or end_season required to calculate seasonal_rain and seasonal_length") }
  
  # cheaper to not save this and to just call it?
  daily <- epicsadata::get_daily_data(country = country, station_id = station_id)
  daily$year <- as.integer(daily$year)
  data_names <- epicsadata::data_definitions(station_id = station_id)
  definitions <- definitions(country = country, station_id = station_id, summaries = summaries)
  summary_data <- expand.grid(year = unique(daily[[data_names$year]]), 
                              station = unique(daily[[data_names$station]]))
  names(summary_data) <- c(data_names$year, data_names$station)
  if ("annual_rain" %in% summaries){
    annual_rain <- rpicsa::annual_rain(daily, date_time = data_names$date, rain = data_names$rain, 
                                       year = data_names$year,  station = data_names$station, 
                                       # how does it know the names of these from the daily_data? Is it always the same?
                                       total_rain = as.logical(definitions$annual_rain$annual_rain),
                                       n_rain = as.logical(definitions$annual_rain$n_rain),
                                       na_rm = as.logical(definitions$annual_rain$na_rm),
                                       na_prop = definitions$annual_rain$na_prop,
                                       na_n = definitions$annual_rain$na_n,
                                       na_consec = definitions$annual_rain$na_consec,
                                       na_n_non = definitions$annual_rain$na_n_non)
    summary_data <- dplyr::full_join(summary_data, annual_rain)
  }
  if ("start_rains" %in% summaries){
    start_rains <- rpicsa::start_rains(daily, date_time = data_names$date, station = data_names$station,  year = data_names$year,  rain = data_names$rain,
                                       #doy = "doy", # todo: sort doy issues about it being calculated
                                       threshold  = definitions$start_rains$threshold,
                                       start_day  = definitions$start_rains$start_day,
                                       end_day = definitions$start_rains$end_day,
                                       output = "doy",
                                       total_rainfall = as.logical(definitions$start_rains$total_rainfall),
                                       over_days = definitions$start_rains$over_days,
                                       amount_rain = definitions$start_rains$amount_rain,
                                       proportion = as.logical(definitions$start_rains$proportion),
                                       number_rain_days = as.logical(definitions$start_rains$number_rain_days),
                                       dry_spell = as.logical(definitions$start_rains$dry_spell),
                                       spell_interval = definitions$start_rains$spell_interval,
                                       spell_max_dry_days = definitions$start_rains$spell_max_dry_days,
                                       dry_period = as.logical(definitions$start_rains$dry_period),
                                       period_interval = definitions$start_rains$period_interval,
                                       max_rain = definitions$start_rains$max_rain,
                                       period_max_dry_days = definitions$start_rains$period_max_dry_days)
    summary_data <- dplyr::full_join(summary_data, start_rains)
    summary_data$start_rains <- as.integer(summary_data$start_rains)
  }
  if ("end_rains" %in% summaries){
    end_rains <- rpicsa::end_rains(daily, date_time = data_names$date, station = data_names$station,  year = data_names$year,  rain = data_names$rain, #doy = "doy",
                                   start_day  = definitions$end_rains$start_day,
                                   end_day = definitions$end_rains$end_day,
                                   output = "doy",
                                   interval_length = definitions$end_rains$interval_length,
                                   min_rainfall = definitions$end_rains$min_rainfall)
    summary_data <- dplyr::full_join(summary_data, end_rains)
    summary_data$end_rains <- as.integer(summary_data$end_rains)
  }
  if ("end_season" %in% summaries){
    end_season <- rpicsa::end_season(daily, date_time = data_names$date, station = data_names$station,
                                     year = data_names$year,
                                     rain = data_names$rain, #doy = "doy",
                                     start_day  = definitions$end_season$start_day,
                                     end_day = definitions$end_season$end_day,
                                     output = "doy",
                                     capacity = definitions$end_season$capacity,
                                     water_balance_max = definitions$end_season$water_balance_max,
                                     evaporation = definitions$end_season$evaporation, # this is a character
                                     evaporation_value = definitions$end_season$evaporation_value,
                                     evaporation_variable = definitions$end_season$evaporation_variable) # todo: evaporation variable as a variable
    summary_data <- dplyr::full_join(summary_data, end_season)
    summary_data$end_season <- as.integer(summary_data$end_season)
  }
  if ("seasonal_rain" %in% summaries){
    present_values <- c("start_rains", "end_rains", "end_season") %in% summaries
    
    if ((length(present_values[present_values]) == 3) || (identical(present_values, c(TRUE, FALSE, TRUE)))){ # if all 3 or sr and es in
      warning("Performing seasonal_rain with end_season") # end_rains might be present too
      season_rain <- rpicsa::seasonal_rain(summary_data = summary_data, start_date = "start_rains", end_date = "end_season", 
                                           daily, date_time = data_names$date, station = data_names$station,  year = data_names$year,  rain = data_names$rain, #doy = "doy",
                                           total_rain = as.logical(definitions$seasonal_rain$seasonal_rain),
                                           n_rain = as.logical(definitions$seasonal_rain$n_rain),
                                           rain_day = definitions$seasonal_rain$rain_day,
                                           na_rm = as.logical(definitions$seasonal_rain$na_rm),
                                           na_prop = definitions$seasonal_rain$na_prop,
                                           na_n = definitions$seasonal_rain$na_n,
                                           na_consec = definitions$seasonal_rain$na_consec,
                                           na_n_non = definitions$seasonal_rain$na_n_non)
    }
    if (identical(present_values, c(TRUE, TRUE, FALSE))){
      # run it but with end_rains
      season_rain <- rpicsa::seasonal_rain(summary_data = summary_data, start_date = "start_rains", end_date = "end_rains", 
                                           daily, date_time = data_names$date, station = data_names$station,  year = data_names$year,  rain = data_names$rain, #doy = "doy",
                                           total_rain = as.logical(definitions$seasonal_rain$seasonal_rain),
                                           n_rain = as.logical(definitions$seasonal_rain$n_rain),
                                           rain_day = definitions$seasonal_rain$rain_day,
                                           na_rm = as.logical(definitions$seasonal_rain$na_rm),
                                           na_prop = definitions$seasonal_rain$na_prop,
                                           na_n = definitions$seasonal_rain$na_n,
                                           na_consec = definitions$seasonal_rain$na_consec,
                                           na_n_non = definitions$seasonal_rain$na_n_non)
      # TODO: rename output to be seasonal_total_rain, seasonal_n_rain, etc.
    }
    summary_data <- dplyr::full_join(summary_data, season_rain)
  }
  if ("seasonal_length" %in% summaries){
    present_values <- c("start_rains", "end_rains", "end_season") %in% summaries
    if ((length(present_values[present_values]) == 3) || (identical(present_values, c(TRUE, FALSE, TRUE)))){ # if all 3 or sr and es in
      # all present
      warning("Performing seasonal_length with end_season") # end_rains might be present too
      season_length <- rpicsa::seasonal_length(summary_data = summary_data, start_date = "start_rains", end_date = "end_season", 
                                               data = daily, date_time = data_names$date, station = data_names$station,  year = data_names$year,  rain = data_names$rain)
    }
    if (identical(present_values, c(TRUE, TRUE, FALSE))){
      # run it but with end_rains
      season_length <- rpicsa::seasonal_length(summary_data = summary_data, start_date = "start_rains", end_date = "end_rains", 
                                               data = daily, date_time = data_names$date, station = data_names$station,  year = data_names$year,  rain = data_names$rain)
    }
    summary_data <- dplyr::full_join(summary_data, season_length)
  }
  list_return <- NULL
  # anything defined in the json to go in here
  # and to be returned in that format (e.g. dataframe, list of lists, etc)
  list_return[[1]] <- c(definitions)
  list_return[[2]] <- summary_data
  return(list_return) # return a list with in it the metadata and the data itself
}
