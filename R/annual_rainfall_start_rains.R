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
  start_rains <- rpicsa::start_rains(daily, date_time = data_names$date, station = data_names$station, year = data_names$year, rain = data_names$rain,
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
