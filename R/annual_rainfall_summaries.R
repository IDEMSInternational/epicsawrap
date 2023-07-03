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
#' #annual_rainfall_summaries(country = "zm", station_id = "16", summaries = c("start_rains", "end_rains", "annual_rain", "seasonal_rain")) #, "end_season"))
annual_rainfall_summaries <- function(country, station_id, summaries = c("annual_rain", "start_rains", "end_rains", "end_season", "seasonal_rain", "seasonal_length")) {
  # Get data definitions and summary definitions
  data_names <- epicsadata::data_definitions(station_id = station_id)
  summaries_1 <- summaries[ !summaries == 'seasonal_length']
  definitions <- definitions(country = country, station_id = station_id, summaries = summaries_1)
  
  # Fetch daily data and preprocess
  daily <- epicsadata::get_daily_data(country = country, station_id = station_id)
  daily$year <- as.integer(daily$year)
  
  # Create summary_data dataframe
  summary_data <- expand.grid(year = unique(daily[[data_names$year]]), station = unique(daily[[data_names$station]]))
  names(summary_data) <- c(data_names$year, data_names$station)
  
  # Calculate summaries
  if ("annual_rain" %in% summaries) {
    if (is.null(definitions$annual_rain$annual_rain)) definitions$annual_rain$annual_rain <- FALSE
    if (is.null(definitions$annual_rain$n_rain)) definitions$annual_rain$n_rain <- FALSE
    if (is.null(definitions$annual_rain$na_rm)){
      warning("Missing value in annual_rain definitions for na_rm. Setting na_rm = FALSE")
      definitions$annual_rain$na_rm <- FALSE
    }
    annual_rain <- rpicsa::annual_rain(daily, date_time = data_names$date, rain = data_names$rain, 
                                       year = data_names$year, station = data_names$station, 
                                       total_rain = as.logical(definitions$annual_rain$annual_rain),
                                       n_rain = as.logical(definitions$annual_rain$n_rain),
                                       na_rm = as.logical(definitions$annual_rain$na_rm),
                                       na_prop = definitions$annual_rain$na_prop,
                                       na_n = definitions$annual_rain$na_n,
                                       na_consec = definitions$annual_rain$na_consec,
                                       na_n_non = definitions$annual_rain$na_n_non)
    summary_data <- dplyr::full_join(summary_data, annual_rain)
  }
  
  # Check if start_rains and end_rains are required for seasonal_rain and seasonal_length
  require_start_rains <- any(grepl("seasonal_", summaries)) & ("start_rains" %in% summaries)
  require_end_rains <- any(grepl("seasonal_", summaries)) & (any(grepl("end_", summaries)))
  
  if ("start_rains" %in% summaries) {
    if (is.null(definitions$start_rains$threshold)) stop("Missing value in start_rains definitions for threshold.")
    if (is.null(definitions$start_rains$start_day)) stop("Missing value in start_rains definitions for start_day.")
    if (is.null(definitions$start_rains$end_day)) stop("Missing value in start_rains definitions for end_day.")
    if (is.null(definitions$start_rains$total_rainfall)){
      definitions$start_rains$total_rainfall <- FALSE
    }
    if (definitions$start_rains$total_rainfall == "TRUE") {
      if (is.null(definitions$start_rains$over_days)) stop("Missing value in start_rains definitions for over_days. over_days needed since total_rainfall = TRUE ")
      if ((as.logical(definitions$start_rains$proportion) == FALSE) && is.null(definitions$start_rains$amount_rain)) stop("Missing value in start_rains definitions for amount_rain. amount_rain needed since total_rainfall = TRUE and proportion = FALSE.")
      if ((as.logical(definitions$start_rains$proportion) == TRUE) && is.null(definitions$start_rains$prob_rain_day)) stop("Missing value in start_rains definitions for amount_rain. prob_rain_day needed since total_rainfall = TRUE and proportion = TRUE.")
    }
    if (is.null(definitions$start_rains$number_rain_days)){
      definitions$start_rains$number_rain_days <- FALSE
    }
    if (definitions$start_rains$number_rain_days == "TRUE") {
      if (is.null(definitions$start_rains$min_rain_days)) stop("Missing value in start_rains definitions for min_rain_days. min_rain_days needed since number_rain_days = TRUE.")
      if (is.null(definitions$start_rains$rain_day_interval)) stop("Missing value in start_rains definitions for rain_day_interval rain_day_interval needed since number_rain_days = TRUE.")
    }
    if (is.null(definitions$start_rains$dry_spell)){
      definitions$start_rains$dry_spell <- FALSE
    }
    if (definitions$start_rains$dry_spell == "TRUE") {
      if (is.null(definitions$start_rains$spell_interval)) stop("Missing value in start_rains definitions for spell_interval. spell_interval needed since dry_spell = TRUE.")
      if (is.null(definitions$start_rains$spell_max_dry_days)) stop("Missing value in start_rains definitions for spell_max_dry_days. spell_max_dry_days needed since dry_spell = TRUE.")
    } 
    if (is.null(definitions$start_rains$dry_period)){
      definitions$start_rains$dry_period <- FALSE
    }
    if (definitions$start_rains$dry_period == "TRUE") {
      if (is.null(definitions$start_rains$period_interval)) stop("Missing value in start_rains definitions for period_interval. period_interval needed since dry_period = TRUE.")
      if (is.null(definitions$start_rains$max_rain)) stop("Missing value in start_rains definitions for max_rain. max_rain needed since dry_period = TRUE.")
      if (is.null(definitions$start_rains$period_max_dry_days)) stop("Missing value in start_rains definitions for period_max_dry_days. period_max_dry_days needed since dry_period = TRUE.")
    } 
    start_rains <- ss(daily, date_time = data_names$date, station = data_names$station, year = data_names$year, rain = data_names$rain,
                                       threshold = definitions$start_rains$threshold,
                                       start_day = definitions$start_rains$start_day,
                                       end_day = definitions$start_rains$end_day,
                                       output = "doy",
                                       total_rainfall = as.logical(definitions$start_rains$total_rainfall),
                                       over_days = definitions$start_rains$over_days,
                                       amount_rain = definitions$start_rains$amount_rain,
                                       proportion = as.logical(definitions$start_rains$proportion),
                                       prob_rain_day = definitions$start_rains$prob_rain_day,
                                       min_rain_days = definitions$start_rains$min_rain_days,
                                       rain_day_interval = definitions$start_rains$rain_day_interval,
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
  
  if ("end_rains" %in% summaries) {
    if (is.null(definitions$end_rains$start_day)) stop("Missing value in end_rains definitions for start_day.")
    if (is.null(definitions$end_rains$end_day)) stop("Missing value in end_rains definitions for end_day.")
    if (is.null(definitions$end_rains$interval_length)) stop("Missing value in end_rains definitions for interval_length.")
    if (is.null(definitions$end_rains$min_rainfall)) stop("Missing value in end_rains definitions for min_rainfall.")
    end_rains <- rpicsa::end_rains(daily, date_time = data_names$date, station = data_names$station, year = data_names$year, rain = data_names$rain,
                                   start_day = definitions$end_rains$start_day,
                                   end_day = definitions$end_rains$end_day,
                                   output = "doy",
                                   interval_length = definitions$end_rains$interval_length,
                                   min_rainfall = definitions$end_rains$min_rainfall)
    summary_data <- dplyr::full_join(summary_data, end_rains)
    summary_data$end_rains <- as.integer(summary_data$end_rains)
  }
  
  if ("end_season" %in% summaries) {
    if (is.null(definitions$end_season$start_day)) stop("Missing value in end_season definitions for start_day.")
    if (is.null(definitions$end_season$end_day)) stop("Missing value in end_season definitions for end_day.")
    if (is.null(definitions$end_season$capacity)) stop("Missing value in end_season definitions for capacity.")
    if (is.null(definitions$end_season$water_balance_max)) stop("Missing value in end_season definitions for water_balance_max.")
    if (is.null(definitions$end_season$evaporation)) stop("Missing value in end_season definitions for evaporation.")
    if (definitions$end_season$evaporation == "value"){
      if (is.null(definitions$end_season$evaporation_value)) stop("Missing value in end_season definitions for evaporation_value.")
    }
    if (definitions$end_season$evaporation == "variable"){
      if (is.null(definitions$end_season$evaporation_variable)) stop("Missing value in end_season definitions for evaporation_variable.")
    }
    
    end_season <- rpicsa::end_season(daily, date_time = data_names$date, station = data_names$station,
                                     year = data_names$year,
                                     rain = data_names$rain,
                                     start_day = definitions$end_season$start_day,
                                     end_day = definitions$end_season$end_day,
                                     output = "doy",
                                     capacity = definitions$end_season$capacity,
                                     water_balance_max = definitions$end_season$water_balance_max,
                                     evaporation = definitions$end_season$evaporation,
                                     evaporation_value = definitions$end_season$evaporation_value,
                                     evaporation_variable = definitions$end_season$evaporation_variable)
    summary_data <- dplyr::full_join(summary_data, end_season)
    summary_data$end_season <- as.integer(summary_data$end_season)
  }
  
  if ("seasonal_rain" %in% summaries) {
    if (require_start_rains && require_end_rains) {
      if (is.null(definitions$seasonal_rain$total_rain)) definitions$seasonal_rain$total_rain <- FALSE
      if (is.null(definitions$seasonal_rain$n_rain)) definitions$seasonal_rain$n_rain <- FALSE
      if (is.null(definitions$seasonal_rain$rain_day)) stop("Missing value in seasonal_rain definitions for rain_day.")
      if (is.null(definitions$seasonal_rain$na_rm)){
        warning("Missing value in seasonal_rain definitions for na_rm. Setting na_rm = FALSE")
        definitions$seasonal_rain$na_rm <- FALSE
      }
      if ("end_season" %in% summaries) {
        warning("Performing seasonal_rain with end_season")
        season_rain <- rpicsa::seasonal_rain(summary_data = summary_data, start_date = "start_rains", end_date = "end_season",
                                             daily, date_time = data_names$date, station = data_names$station, year = data_names$year, rain = data_names$rain,
                                             total_rain = as.logical(definitions$seasonal_rain$seasonal_rain),
                                             n_rain = as.logical(definitions$seasonal_rain$n_rain),
                                             rain_day = definitions$seasonal_rain$rain_day,
                                             na_rm = as.logical(definitions$seasonal_rain$na_rm),
                                             na_prop = definitions$seasonal_rain$na_prop,
                                             na_n = definitions$seasonal_rain$na_n,
                                             na_consec = definitions$seasonal_rain$na_consec,
                                             na_n_non = definitions$seasonal_rain$na_n_non)
      } else if ("end_rains" %in% summaries) {
        warning("Performing seasonal_rain with end_rains")
        season_rain <- rpicsa::seasonal_rain(summary_data = summary_data, start_date = "start_rains", end_date = "end_rains",
                                             daily, date_time = data_names$date, station = data_names$station, year = data_names$year, rain = data_names$rain,
                                             total_rain = as.logical(definitions$seasonal_rain$seasonal_rain),
                                             n_rain = as.logical(definitions$seasonal_rain$n_rain),
                                             rain_day = definitions$seasonal_rain$rain_day,
                                             na_rm = as.logical(definitions$seasonal_rain$na_rm),
                                             na_prop = definitions$seasonal_rain$na_prop,
                                             na_n = definitions$seasonal_rain$na_n,
                                             na_consec = definitions$seasonal_rain$na_consec,
                                             na_n_non = definitions$seasonal_rain$na_n_non)
      }
      summary_data <- dplyr::full_join(summary_data, season_rain)
    } else {
      stop("start_rains and at least one of end_season or end_rains is required to calculate seasonal_rain")
    }
  }
  
  if ("seasonal_length" %in% summaries) {
    if (require_start_rains && require_end_rains) {
      if ("end_season" %in% summaries) {
        warning("Performing seasonal_length with end_season")
        season_length <- rpicsa::seasonal_length(summary_data = summary_data, start_date = "start_rains", end_date = "end_season",
                                                 data = daily, date_time = data_names$date, station = data_names$station, year = data_names$year, rain = data_names$rain)
      } else {
        warning("Performing seasonal_length with end_rains")
        season_length <- rpicsa::seasonal_length(summary_data = summary_data, start_date = "start_rains", end_date = "end_rains",
                                                 data = daily, date_time = data_names$date, station = data_names$station, year = data_names$year, rain = data_names$rain)
      }
      summary_data <- dplyr::full_join(summary_data, season_length)
    } else {
      stop("start_rains and at least one of end_season or end_rains is required to calculate seasonal_length")
    }
  }
  
  list_return <- NULL
  list_return[[1]] <- c(definitions)
  list_return[[2]] <- summary_data
  return(list_return)
}