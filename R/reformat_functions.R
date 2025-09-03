#' Reformat Annual Climate Summaries
#'
#' This function standardizes column names and converts data types for annual climate summaries.
#' It prepares the dataset for further processing or export, ensuring consistency in variable naming
#' and data formats (e.g., converting dates, rain counts, and DOY values).
#'
#' @param data A data frame containing annual summaries.
#' @param station_col Name of the column containing station information.
#' @param year_col Name of the column containing year information.
#' @param start_rains_doy_col Column for start of rains (day-of-year).
#' @param start_rains_date_col Column for start of rains (date).
#' @param end_rains_doy_col Column for end of rains (day-of-year).
#' @param end_rains_date_col Column for end of rains (date).
#' @param end_season_doy_col Column for end of season (day-of-year).
#' @param end_season_date_col Column for end of season (date).
#' @param seasonal_rain_col Column for total seasonal rainfall.
#' @param n_seasonal_rain_col Column for number of seasonal rain days.
#' @param season_length_col Column for season length (days).
#' @param annual_rain_col Column for total annual rainfall.
#' @param n_rain_col Column for total number of rain days.
#' @param extreme_rain_days_col Column for number of extreme rain days.
#' @param extreme_tmin_days_col Column for number of extreme tmin days.
#' @param extreme_tmax_days_col Column for number of extreme tmax days.
#' @param longest_rain_spell_col Column for the longest rainfall spell.
#' @param longest_tmin_spell_col Column for the longest tmin spell.
#' @param longest_tmax_spell_col Column for the longest tmax spell.
#'
#' @return A reformatted data frame with standardized column names and cleaned data types.
#' @export
reformat_annual_summaries <- function(data,
                                      station_col = NULL, year_col = NULL,
                                      start_rains_doy_col = NULL, start_rains_date_col = NULL,
                                      end_rains_doy_col = NULL, end_rains_date_col = NULL,
                                      end_season_doy_col = NULL, end_season_date_col = NULL,
                                      seasonal_rain_col = NULL, n_seasonal_rain_col = NULL,
                                      season_length_col = NULL, annual_rain_col = NULL,
                                      n_rain_col = NULL,
                                      extreme_rain_days_col = NULL,
                                      extreme_tmin_days_col = NULL, extreme_tmax_days_col = NULL,
                                      longest_rain_spell_col = NULL, longest_tmin_spell_col = NULL, longest_tmax_spell_col = NULL) {
  # Rename columns
  data <- data %>%
    dplyr::select(dplyr::any_of(c(station_col, year_col, start_rains_doy_col, start_rains_date_col,
                           end_rains_doy_col, end_rains_date_col, end_season_doy_col,
                           end_season_date_col, seasonal_rain_col, n_seasonal_rain_col,
                           season_length_col, annual_rain_col, n_rain_col, extreme_rain_days_col, 
                           extreme_tmin_days_col, extreme_tmax_days_col, longest_rain_spell_col, 
                           longest_tmin_spell_col, longest_tmax_spell_col))) %>%
    dplyr::rename(
      station = {{station_col}},
      year = {{year_col}},
      start_rains_doy = {{start_rains_doy_col}},
      start_rains_date = {{start_rains_date_col}},
      end_rains_doy = {{end_rains_doy_col}},
      end_rains_date = {{end_rains_date_col}},
      end_season_doy = {{end_season_doy_col}},
      end_season_date = {{end_season_date_col}},
      seasonal_rain = {{seasonal_rain_col}},
      n_seasonal_rain = {{n_seasonal_rain_col}},
      season_length = {{season_length_col}},
      annual_rain = {{annual_rain_col}},
      n_rain = {{n_rain_col}},
      extreme_rain_days = {{extreme_rain_days_col}},
      extreme_tmin_days = {{extreme_tmin_days_col}},
      extreme_tmax_days = {{extreme_tmax_days_col}},
      long_spell_rains = {{longest_rain_spell_col}},
      long_spell_tmin = {{longest_tmin_spell_col}},
      long_spell_tmax = {{longest_tmax_spell_col}}
    )
  
  # Convert station to factor if it's not already
  if (!is.null(data$station) && !is.factor(data$station)) {
    data$station <- as.factor(data$station)
  }
  if (!is.null(data$year) && !is.factor(data$year)) {
    data$year <- as.factor(data$year)
  }
  
  data <- data %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("_doy"), as.integer),
           dplyr::across(dplyr::ends_with("_date"), as.character),
           dplyr::across(dplyr::ends_with("_rain"), as.integer),
           dplyr::across(dplyr::starts_with("n_"), as.double),
    )
  
  if (!is.null(station_col)) data <- data %>% dplyr::mutate(station = as.character(station))
  return(data)
}

#' Reformat crop success data
#'
#' This function reformats crop success data by renaming columns and converting data types.
#' 
#' @param data A data frame containing the crop success data.
#' @param station_col Name of the column containing station information.
#' @param total_rain_col Name of the column containing total rain data.
#' @param plant_day_col Name of the column containing plant day data.
#' @param plant_length_col Name of the column containing plant length data.
#' @param prop_success_with_start_col Name of the column containing proportion of success data (with start as `TRUE`).
#' @param prop_success_no_start_col Name of the column containing proportion of success data (with start as `FALSE`).
#'
#' @return The reformatted data frame.
#' @export
reformat_crop_success <- function(data, station_col = NULL, total_rain_col, plant_day_col,
                                  plant_length_col, prop_success_with_start_col, prop_success_no_start_col) {
  # Rename columns
  data <- data %>%
    dplyr::rename(
      station = {{station_col}},
      total_rain = {{total_rain_col}},
      plant_day = {{plant_day_col}},
      plant_length = {{plant_length_col}},
      prop_success_with_start = {{prop_success_with_start_col}},
      prop_success_no_start = {{prop_success_no_start_col}}
    )
  
  # Convert station to factor if it's not already
  if (!is.null(data$station) && !is.factor(data$station)) {
    data$station <- as.factor(data$station)
  }
  
  # Convert total_rain, plant_day, plant_length to integer if they're not already
  data$total_rain <- as.integer(data$total_rain)
  data$plant_day <- as.integer(data$plant_day)
  data$plant_length <- as.integer(data$plant_length)
  
  # Convert prop_success to double if it's not already
  data$prop_success_with_start <- as.double(data$prop_success_with_start)
  data$prop_success_no_start <- as.double(data$prop_success_no_start)
  
  if (!is.null(station_col)) data <- data %>% dplyr::mutate(station = as.character(station))
  return(data)
}

#' Reformat temperature summaries data
#'
#' This function reformats temperature summaries data by renaming columns and converting data types.
#' 
#' @param data A data frame containing the temperature summaries data.
#' @param station_col Name of the column containing station information.
#' @param year_col Name of the column containing year information.
#' @param month_col Name of the column containing month information.
#' @param mean_tmin_col Name of the column containing mean minimum temperature data.
#' @param min_tmin_col Name of the column containing minimum minimum temperature data.
#' @param max_tmin_col Name of the column containing maximum minimum temperature data.
#' @param mean_tmax_col Name of the column containing mean maximum temperature data.
#' @param min_tmax_col Name of the column containing minimum maximum temperature data.
#' @param max_tmax_col Name of the column containing maximum maximum temperature data.
#'
#' @return The reformatted data frame.
#' @export
reformat_temperature_summaries <- function(data,
                                           station_col = NULL, year_col = NULL, month_col = NULL,
                                           mean_tmin_col = NULL, min_tmin_col = NULL, max_tmin_col = NULL,
                                           mean_tmax_col = NULL, min_tmax_col = NULL, max_tmax_col = NULL) {
  # Rename columns
  data <- data %>%
    dplyr::select(dplyr::any_of(c(station_col, year_col, month_col, mean_tmin_col,
                           min_tmin_col, max_tmin_col, mean_tmax_col,
                           min_tmax_col, max_tmax_col))) %>%
    dplyr::rename(
      station = {{station_col}},
      year = {{year_col}},
      month = {{month_col}},
      mean_tmin = {{mean_tmin_col}},
      min_tmin = {{min_tmin_col}},
      max_tmin = {{max_tmin_col}},
      mean_tmax = {{mean_tmax_col}},
      min_tmax = {{min_tmax_col}},
      max_tmax = {{max_tmax_col}}
    )
  
  # Convert station to factor if it's not already
  if (!is.null(data$station) && !is.factor(data$station)) {
    data$station <- as.factor(data$station)
  }
  # if (!is.null(data$year) && !is.factor(data$year)) {
  #   data$year <- as.factor(data$year)
  # }
  if (!is.null(station_col)) data <- data %>% dplyr::mutate(station = as.character(station))
  return(data)
}

#' Reformat season start data
#'
#' This function reformats season start data by renaming columns and calculating proportions.
#' 
#' @param data A data frame containing the season start data.
#' @param station_col Name of the column containing station information.
#' @param year_col Name of the column containing year information.
#' @param plant_day_col Name of the column containing plant day data.
#' @param plant_day_cond_col Name of the column containing plant day condition data.
#'
#' @return The reformatted data frame.
#' @export
reformat_season_start <- function(data, station_col = NULL, year_col, plant_day_col, plant_day_cond_col){
  data <- data %>%
    dplyr::select(dplyr::any_of(c(station = station_col,
                           year = year_col,
                           day = plant_day_col,
                           plant_day_cond = plant_day_cond_col))) %>%
    unique() %>%
    dplyr::group_by(station, day = as.integer(day)) %>%
    dplyr::summarise(proportion = sum(plant_day_cond, na.rm = TRUE)/dplyr::n())
  if (!is.null(station_col)) data <- data %>% dplyr::mutate(station = as.character(station))
  return(data)
}