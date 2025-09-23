#' Calculate Annual Rainfall
#'
#' This function calculates annual rainfall based on the provided definitions,
#' daily rainfall data, and data variable names.
#'
#' @param definitions A list containing definitions for the annual rainfall analysis,
#'        including options for calculating total rainfall, number of rainy days,
#'        and criteria for handling missing values.
#' @param daily A data frame containing daily rainfall data.
#' @param data_names A list of variable names used in the data frame.
#'
#' @return A numeric vector representing annual rainfall and related calculations.
#'
#' @examples
#' 
#'# # Example usage:
#'# require(dplyr)
#'# library(cdms.products)
#'# data(daily_niger)
#'# definitions <- list(
#'#   annual_rain = list(
#'#     annual_rain = TRUE,
#'#     n_rain = FALSE,
#'#     na_rm = TRUE
#'#   )
#'# )
#'# data_names <- list(date = "date", station = "station_name", year = "year", rain = "rain")
#'# daily_data <- daily_niger %>% dplyr::filter(year > 1975) %>% dplyr::filter(station_name == "Zinder")
#'# annual_rainfall <- annual_rainfall_annual_rain(definitions, daily_data, data_names)
#'
annual_rainfall_annual_rain <- function(definitions, daily, data_names) {
  if (is.null(definitions$annual_rain$annual_rain)) 
    definitions$annual_rain$annual_rain <- "FALSE"
  if (definitions$annual_rain$n_rain){
    if (is.null(definitions$annual_rain$rain_day)) {
      definitions$annual_rain$rain_day <- 0
    }
  } else {
    definitions$annual_rain$rain_day <- NA
  }
  if (is.null(definitions$annual_rain$na_rm)){
    warning("Missing value in annual_rain definitions for na_rm. Setting na_rm = FALSE")
    definitions$annual_rain$na_rm <- "FALSE"
  }
  if (!is.null(definitions$annual_rain$na_prop) && definitions$annual_rain$na_prop > 1){
    na_prop <- definitions$annual_rain$na_prop/100
  } else {
    na_prop <- NULL
  }
  if (is.null(as.logical(definitions$annual_rain$annual_rain))){
    total_rain <- as.logical(definitions$annual_rain$total_rain)
  } else {
    total_rain <- as.logical(definitions$annual_rain$annual_rain)
  }
  annual_rain <- rpicsa::annual_rain(
    daily, 
    date_time = data_names$date, 
    rain = data_names$rain, 
    year = data_names$year, 
    station = data_names$station, 
    s_start_doy = definitions$annual_rain$s_start_doy,
    total_rain = total_rain,
    n_rain = as.logical(definitions$annual_rain$n_rain),
    rain_day = definitions$annual_rain$rain_day,
    na_rm = as.logical(definitions$annual_rain$na_rm),
    na_prop = na_prop,
    na_n = definitions$annual_rain$na_n,
    na_consec = definitions$annual_rain$na_consec,
    na_n_non = definitions$annual_rain$na_n_non
  )
  return(annual_rain)
}

#' Calculate End of Rains
#'
#' This function calculates the end of the rainy season based on the provided definitions,
#' daily rainfall data, and data variable names.
#'
#' @param definitions A list containing definitions for the end of the rainy season analysis,
#'        including start and end days, interval length, and minimum rainfall criteria.
#' @param daily A data frame containing daily rainfall data.
#' @param data_names A list of variable names used in the data frame.
#'
#' @return A numeric vector representing the end of the rainy season in terms of day of the year (DOY).
#'
#' @export
#'
#' @examples
#' 
#' # Example usage:
#'# require(dplyr)
#'# library(cdms.products)
#'# data(daily_niger)
#'# definitions <- list(
#'#   end_rains = list(
#'#     start_day = 122,
#'#     end_day = 366,
#'#     interval_length = 7,
#'#     min_rainfall = 5
#'#   )
#'# )
#'# data_names <- list(date = "date", station = "station_name", year = "year", rain = "rain")
#'# daily_data <- daily_niger %>% dplyr::filter(year > 1975) %>% dplyr::filter(station_name == "Zinder")
#'# end_of_rains <- annual_rainfall_end_rains(definitions, daily_data, data_names)
#'
annual_rainfall_end_rains <- function(definitions, daily, data_names) {
  if (is.null(definitions$end_rains$start_day)) 
    stop("Missing value in end_rains definitions for start_day.")
  if (is.null(definitions$end_rains$end_day)) 
    stop("Missing value in end_rains definitions for end_day.")
  if (is.null(definitions$end_rains$interval_length)) 
    stop("Missing value in end_rains definitions for interval_length.")
  if (is.null(definitions$end_rains$min_rainfall)) 
    stop("Missing value in end_rains definitions for min_rainfall.")
  
  end_rains <- rpicsa::end_rains(
    daily, 
    date_time = data_names$date, 
    station = data_names$station, 
    year = data_names$year, 
    rain = data_names$rain,
    start_day = definitions$end_rains$start_day,
    end_day = definitions$end_rains$end_day,
    s_start_doy = definitions$end_rains$s_start_doy,
    output = "both",
    interval_length = definitions$end_rains$interval_length,
    min_rainfall = definitions$end_rains$min_rainfall
  )
  return(end_rains)
}

#' Calculate End of Season
#'
#' This function calculates the end of the rainy season based on the provided definitions,
#' daily rainfall data, and data variable names.
#'
#' @param definitions A list containing definitions for the end of the rainy season analysis,
#'        including start and end days, capacity, water balance, evaporation criteria,
#'        and related parameters.
#' @param daily A data frame containing daily rainfall data.
#' @param data_names A list of variable names used in the data frame.
#'
#' @return A numeric vector representing the end of the rainy season in terms of day of the year (DOY).
#'
#' @export
#'
#' @examples
#' # Example usage:
#'# require(dplyr)
#'# library(cdms.products)
#'# data(daily_niger)
#'# definitions <- list(
#'#   end_season = list(
#'#     start_day = 150,
#'#     end_day = 366,
#'#     capacity = 100,
#'#     water_balance_max = 50,
#'#     evaporation = "value",
#'#     evaporation_value = 10
#'#   )
#'# )
#'# data_names <- list(date = "date", station = "station_name", year = "year", rain = "rain")
#'# daily_data <- daily_niger %>% dplyr::filter(year > 1975) %>% dplyr::filter(station_name == "Zinder")
#'# end_of_season <- annual_rainfall_end_season(definitions, daily_data, data_names)
#'
annual_rainfall_end_season <- function(definitions, daily, data_names) {
  if (is.null(definitions$end_season$start_day)) 
    stop("Missing value in end_season definitions for start_day.")
  if (is.null(definitions$end_season$end_day)) 
    stop("Missing value in end_season definitions for end_day.")
  if (is.null(definitions$end_season$capacity)) 
    stop("Missing value in end_season definitions for capacity.")
  if (is.null(definitions$end_season$water_balance_max)) 
    stop("Missing value in end_season definitions for water_balance_max.")
  if (is.null(definitions$end_season$evaporation)) 
    stop("Missing value in end_season definitions for evaporation.")
  if (definitions$end_season$evaporation == "value"){
    if (is.null(definitions$end_season$evaporation_value)) 
      stop("Missing value in end_season definitions for evaporation_value.")
  }
  if (definitions$end_season$evaporation == "variable"){
    if (is.null(definitions$end_season$evaporation_variable)) 
      stop("Missing value in end_season definitions for evaporation_variable.")
  }
  
  end_season <- rpicsa::end_season(
    daily, 
    date_time = data_names$date, 
    station = data_names$station,
    year = data_names$year,
    rain = data_names$rain,
    start_day = definitions$end_season$start_day,
    end_day = definitions$end_season$end_day,
    s_start_doy = definitions$end_season$s_start_doy,
    output = "both",
    capacity = definitions$end_season$capacity,
    water_balance_max = definitions$end_season$water_balance_max,
    evaporation = definitions$end_season$evaporation,
    evaporation_value = definitions$end_season$evaporation_value,
    evaporation_variable = definitions$end_season$evaporation_variable
  )
  return(end_season)
}

#' Calculate Seasonal Length
#'
#' This function calculates the seasonal length of annual rainfall based on the
#' provided definitions, daily rainfall data, summary data, data variable names,
#' and summary types.
#'
#' @param definitions A list containing definitions for various aspects of the analysis.
#' @param daily A data frame containing daily rainfall data.
#' @param summary_data A data frame containing summary data.
#' @param data_names A list of variable names used in the data frames.
#' @param summaries A character vector specifying the summary types.
#'
#' @return A numeric vector representing the seasonal length of annual rainfall.
#'
#' @export
#'
#' @examples
#' 
#' # Example usage:
#' definitions <- list(seasonal_length = list(end_type = "rains"))
#' data_names <- list(date = "date", station = "station", year = "year", rain = "rain")
#' summary_types <- c("start_rains", "end_rains", "end_season")
#' daily_data <- data.frame(date = seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "days"),
#'                          station = "Station_A",
#'                          year = rep(2023, each = 365),
#'                          rain = runif(365, min = 0, max = 10))
#' summary_data <- data.frame(year = 2023, start_rains_date = as.Date("2023-04-01"), 
#'                            end_rains_date = as.Date("2023-10-31"), end_season_date = as.Date("2023-12-31"))
#' #seasonal_length <- annual_rainfall_seasonal_length(definitions, daily_data, summary_data, data_names, summary_types)
annual_rainfall_seasonal_length <- function(definitions, daily, summary_data, data_names, summaries) {
  if (is.null(definitions$seasonal_length$end_type)) {
    present_values <- c("start_rains", "end_rains", "end_season") %in% summaries
    if ((length(present_values[present_values]) == 3) || (identical(present_values, c(TRUE, FALSE, TRUE)))) {
      warning("Performing seasonal_length with end_season")
      end_date <- "end_season_date"
    }
    if (identical(present_values, c(TRUE, TRUE, FALSE))) end_date <- "end_rains_date"
  } else {
    if (definitions$seasonal_length$end_type == "rains") {
      end_date <- "end_rains_date"
    } else {
      end_date <- "end_season_date"
    } 
  }
  season_length <- rpicsa::seasonal_length(
    summary_data = summary_data, 
    start_date = "start_rains_date", 
    end_date = end_date, 
    data = daily, 
    date_time = data_names$date, 
    station = data_names$station, 
    year = data_names$year, 
    rain = data_names$rain
  )
  return(season_length)
}

#' Calculate Seasonal Rain
#'
#' This function calculates the seasonal rainfall based on the provided definitions, 
#' daily rainfall data, summary data, data variable names, and summary types.
#'
#' @param definitions A list containing definitions for various aspects of the analysis,
#'        including seasonal rainfall properties such as total rain, number of rainy days,
#'        and criteria for rainy days.
#' @param daily A data frame containing daily rainfall data.
#' @param summary_data A data frame containing summary data.
#' @param data_names A list of variable names used in the data frames.
#' @param summaries A character vector specifying the summary types.
#'
#' @return A numeric vector representing the seasonal rainfall based on the provided criteria.
#'
#' @export
#'
#' @examples
#' 
#'# # Example usage:
#'# definitions <- list(
#'#   seasonal_rain = list(
#'#     total_rain = TRUE,
#'#     n_rain = TRUE,
#'#     rain_day = 10,
#'#     na_rm = TRUE,
#'#     end_type = "rains"
#'#   )
#'# )
#'# data_names <- list(date = "date", station = "station", year = "year", rain = "rain")
#'# summary_types <- c("start_rains", "end_rains", "end_season")
#'# daily_data <- data.frame(date = seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "days"),
#'#                          station = "Station_A",
#'#                          year = rep(2023, each = 365),
#'#                          rain = runif(365, min = 0, max = 10))
#'# daily_data$doy <- lubridate::yday(daily_data$date)
#'# summary_data <- data.frame(year = 2023, start_rains = as.Date("2023-04-01"), 
#'#                            end_rains = as.Date("2023-10-31"), end_season = as.Date("2023-12-31"))
#'# seasonal_rain <- annual_rainfall_seasonal_rain(definitions, daily_data, summary_data, data_names, summary_types)
#'
annual_rainfall_seasonal_rain <- function(definitions, daily, summary_data, data_names, summaries) {
  if (is.null(definitions$seasonal_rain$total_rain)) definitions$seasonal_rain$total_rain <- "FALSE"
  if (is.null(definitions$seasonal_rain$n_rain)) definitions$seasonal_rain$n_rain <- "FALSE"
  if (definitions$seasonal_rain$n_rain){
    if (is.null(definitions$seasonal_rain$rain_day)) {
      warning("Missing value in seasonal_rain definitions for rain_day. Setting as 0")
      definitions$seasonal_rain$rain_day <- 0
    }
  } else {
    definitions$seasonal_rain$rain_day <- NA
  }
  if (is.null(definitions$seasonal_rain$na_rm)){
    warning("Missing value in seasonal_rain definitions for na_rm. Setting na_rm = FALSE")
    definitions$seasonal_rain$na_rm <- "FALSE"
  }
  if (is.null(definitions$seasonal_rain$end_type)){
    present_values <- c("start_rains", "end_rains", "end_season") %in% summaries
    if ((length(present_values[present_values]) == 3) || (identical(present_values, c(TRUE, FALSE, TRUE)))) {
      warning("Performing seasonal_rain with end_season")
      end_date <- "end_season_date"
    }
    if (identical(present_values, c(TRUE, TRUE, FALSE))) end_date <- "end_rains_date"
  } else {
    if (definitions$seasonal_rain$end_type == "rains"){
      end_date <- "end_rains_date"
    } else {
      end_date <- "end_season_date"
    } 
  }
  if (!is.null(definitions$seasonal_rain$na_prop) && definitions$seasonal_rain$na_prop > 1){
    na_prop <- definitions$seasonal_rain$na_prop/100
  } else {
    na_prop <- NULL
  }
  season_rain <- rpicsa::seasonal_rain(
    summary_data = summary_data, 
    start_date = "start_rains_date", 
    end_date = end_date, 
    data = daily, 
    date_time = data_names$date, 
    station = data_names$station, 
    year = data_names$year, 
    rain = data_names$rain, 
    s_start_doy = definitions$start_rains$s_start_doy,
    total_rain = as.logical(definitions$seasonal_rain$total_rain), 
    n_rain = as.logical(definitions$seasonal_rain$n_rain), 
    rain_day = definitions$seasonal_rain$rain_day, 
    na_rm = as.logical(definitions$seasonal_rain$na_rm), 
    na_prop = na_prop, 
    na_n = definitions$seasonal_rain$na_n, 
    na_consec = definitions$seasonal_rain$na_consec, 
    na_n_non = definitions$seasonal_rain$na_n_non
  )
  return(season_rain)
}

#' Calculate Annual Rainfall Start of Rains
#'
#' This function calculates the start of the rainy season based on the provided definitions,
#' daily rainfall data, and data variable names.
#'
#' @param definitions A list containing definitions for the start of the rainy season analysis,
#'        including threshold, start and end days, and various options for criteria and calculations.
#' @param daily A data frame containing daily rainfall data.
#' @param data_names A list of variable names used in the data frame.
#'
#' @return A numeric vector representing the start of the rainy season in terms of day of the year (DOY).
#'
#' @export
#'
#' @examples
#'# require(dplyr)
#'# library(cdms.products)
#'# data(daily_niger)
#'# # Example usage:
#'# definitions <- list(
#'#   start_rains = list(
#'#     threshold = 0.5,
#'#     start_day = 90,
#'#     end_day = 200,
#'#     total_rainfall = TRUE,
#'#     over_days = 7,
#'#     amount_rain = 10,
#'#     proportion = FALSE,
#'#     prob_rain_day = 0.6,
#'#     number_rain_days = TRUE,
#'#     min_rain_days = 3,
#'#     rain_day_interval = 5,
#'#     dry_spell = TRUE,
#'#     spell_interval = 14,
#'#     spell_max_dry_days = 10,
#'#     dry_period = FALSE
#'#   )
#'# )
#'# data_names <- list(date = "date", station = "station_name", year = "year", rain = "rain")
#'# daily_data <- daily_niger %>% dplyr::filter(year > 1975) %>% dplyr::filter(station_name == "Zinder")
#'# start_of_rains <- annual_rainfall_start_rains(definitions, daily_data, data_names)
#'
annual_rainfall_start_rains <- function(definitions, daily, data_names){
  if (is.null(definitions$start_rains$threshold)) {
    stop("Missing value in start_rains definitions for threshold.")
  }
  if (is.null(definitions$start_rains$start_day)) {
    stop("Missing value in start_rains definitions for start_day.")
  }
  if (is.null(definitions$start_rains$end_day)) {
    stop("Missing value in start_rains definitions for end_day.")
  }
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
  start_rains <- rpicsa::start_rains(daily,
                                     date_time = data_names$date,
                                     station = data_names$station,
                                     year = data_names$year,
                                     rain = data_names$rain,
                                     threshold = definitions$start_rains$threshold,
                                     start_day = definitions$start_rains$start_day,
                                     end_day = definitions$start_rains$end_day,
                                     s_start_doy = definitions$start_rains$s_start_doy,
                                     output = "both",
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

  return(start_rains)
}