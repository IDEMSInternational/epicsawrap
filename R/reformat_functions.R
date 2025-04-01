#' Reformat annual summaries data
#'
#' This function reformats annual summaries data by renaming columns and converting data types.
#' 
#' @param data A data frame containing the annual summaries data.
#' @param station_col Name of the column containing station information.
#' @param year_col Name of the column containing year information.
#' @param start_rains_doy_col Name of the column containing start of rains day of year.
#' @param start_rains_date_col Name of the column containing start of rains date.
#' @param end_rains_doy_col Name of the column containing end of rains day of year.
#' @param end_rains_date_col Name of the column containing end of rains date.
#' @param end_season_doy_col Name of the column containing end of season day of year.
#' @param end_season_date_col Name of the column containing end of season date.
#' @param seasonal_rain_col Name of the column containing seasonal rain data.
#' @param n_seasonal_rain_col Name of the column containing number of seasonal rain events.
#' @param season_length_col Name of the column containing season length.
#' @param annual_rain_col Name of the column containing annual rain data.
#' @param n_rain_col Name of the column containing number of rain events.
#'
#' @return The reformatted data frame.
#' @export
reformat_annual_summaries <- function(data,
                                      station_col = NULL, year_col = NULL,
                                      start_rains_doy_col = NULL, start_rains_date_col = NULL,
                                      end_rains_doy_col = NULL, end_rains_date_col = NULL,
                                      end_season_doy_col = NULL, end_season_date_col = NULL,
                                      seasonal_rain_col = NULL, n_seasonal_rain_col = NULL,
                                      season_length_col = NULL, annual_rain_col = NULL,
                                      n_rain_col = NULL) {
  # Rename columns
  data <- data %>%
    dplyr::select(dplyr::any_of(c(station_col, year_col, start_rains_doy_col, start_rains_date_col,
                           end_rains_doy_col, end_rains_date_col, end_season_doy_col,
                           end_season_date_col, seasonal_rain_col, n_seasonal_rain_col,
                           season_length_col, annual_rain_col, n_rain_col))) %>%
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
      n_rain = {{n_rain_col}}
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
#' @param prop_success_col Name of the column containing proportion of success data.
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
  return(data)
}