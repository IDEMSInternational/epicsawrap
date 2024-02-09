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
  definitions <- definitions(country = country, station_id = station_id, summaries = summaries)
  definitions_season <- NULL
  # Fetch daily data and preprocess
  daily <- epicsadata::get_daily_data(country = country, station_id = station_id)
  # For the variable names to be set as a certain default, set TRUE here, and run check_and_rename_variables
  data_names <- epicsadata::data_definitions(names(daily), TRUE)
  daily <- check_and_rename_variables(daily, data_names)
  daily$year <- as.integer(daily$year)
  
  # Check if start_rains and end_rains are required for seasonal_rain and seasonal_length
  require_start_rains <- any(grepl("seasonal_", summaries)) & ("start_rains" %in% summaries)
  require_end_rains <- any(grepl("seasonal_", summaries)) & (any(grepl("end_", summaries)))
  
  summary_data <- NULL
  
  # Calculate summaries ==================================================================
  if ("start_rains" %in% summaries) {
    start_rains <- annual_rainfall_start_rains(definitions, daily, data_names)
    summary_data <- join_null_data(summary_data, start_rains)
    summary_data$start_rains_doy <- as.integer(summary_data$start_rains_doy)
  }
  if ("end_rains" %in% summaries) {
    if (!is.null(definitions$start_rains$s_start_doy)) definitions$end_rains$s_start_doy <- definitions$start_rains$s_start_doy
    end_rains <- annual_rainfall_end_rains(definitions, daily, data_names)
    summary_data <- join_null_data(summary_data, end_rains)
    summary_data$end_rains_doy <- as.integer(summary_data$end_rains_doy)
  }

  if ("end_season" %in% summaries) {
    if (!is.null(definitions$start_rains$s_start_doy)) definitions$end_season$s_start_doy <- definitions$start_rains$s_start_doy
    end_season <- annual_rainfall_end_season(definitions, daily, data_names)
    summary_data <- join_null_data(summary_data, end_season)
    summary_data$end_season_doy <- as.integer(summary_data$end_season_doy)
  } 
  
  if ("seasonal_rain" %in% summaries) {
    if (require_start_rains && require_end_rains) {
      season_rain <- annual_rainfall_seasonal_rain(definitions, daily, summary_data, data_names, summaries)
      summary_data <- join_null_data(summary_data, season_rain)
    } else {
      definitions_season <- definitions(country = country, station_id = station_id, summaries = c("start_rains", "end_rains", "end_season"))
      if (!require_start_rains){ # if start of rains is not given then ...
        warning("Creating start_rains column to calculate seasonal summaries")
        # check for start_rains in definitions file
        if ("start_rains" %in% names(definitions_season)){
          start_rains <- annual_rainfall_start_rains(definitions_season, daily, data_names)
          summary_data <- join_null_data(summary_data, start_rains)
          summary_data$start_rains <- as.integer(summary_data$start_rains_doy)
          summaries <- c(summaries, "start_rains")
        } else {
          stop("Cannot calculate seasonal_rain without start_rains in definitions file.")
        }
        require_start_rains <- TRUE
      }
      if (!require_end_rains){
        # check for end_rains/seasons in definitions file
        # and checking for preferences given in seasonal_rain
        if (is.null(definitions$seasonal_rain$end_type)){
          if ("end_season" %in% names(definitions_season)){
            end_type <- "seasons"
          } else if ("end_rains" %in% names(definitions_season)){
            end_type <- "rains"
          } else {
            stop("Cannot calculate seasonal_rain without end_rains or end_season in definitions file.")
          }
        } else {
          end_type <- definitions$seasonal_rain$end_type
        }
        if (end_type == "seasons"){
          warning("Creating end_season column to calculate seasonal summaries")
          if (!is.null(definitions$start_rains$s_start_doy)) definitions$end_season$s_start_doy <- definitions$start_rains$s_start_doy
          end_season <- annual_rainfall_end_season(definitions_season, daily, data_names)
          summary_data <- join_null_data(summary_data, end_season)
          summary_data$end_season <- as.integer(summary_data$end_season_doy)
          summaries <- c(summaries, "end_season")
        } else { # run for rains
          warning("Creating end_rains column to calculate seasonal summaries")
          if (!is.null(definitions$start_rains$s_start_doy)) definitions$end_rains$s_start_doy <- definitions$start_rains$s_start_doy
          end_rains <- annual_rainfall_end_rains(definitions_season, daily, data_names)
          summary_data <- join_null_data(summary_data, end_rains)
          summary_data$end_rains <- as.integer(summary_data$end_rains_doy)
          summaries <- c(summaries, "end_rains")
        }
        require_end_rains <- TRUE
      }
      definitions <- c(definitions, definitions_season)
      names_definitions <- unique(names(definitions))
      definitions <- unique(definitions)
      names(definitions) <- names_definitions
      season_rain <- annual_rainfall_seasonal_rain(definitions, daily, summary_data, data_names, summaries)
      summary_data <- join_null_data(summary_data, season_rain)
    }
  }
  
  if ("seasonal_length" %in% summaries) {
    if (require_start_rains && require_end_rains) {
      season_length <- annual_rainfall_seasonal_length(definitions, daily, summary_data, data_names, summaries)
      summary_data <- join_null_data(summary_data, season_length)
    } else {
      definitions_season <- definitions(country = country, station_id = station_id, summaries = c("start_rains", "end_rains", "end_season"))
      if (!require_start_rains){ # if start of rains is not given then ...
        warning("Creating start_rains column to calculate seasonal summaries")
        # check for start_rains in definitions file
        if ("start_rains" %in% names(definitions_season)){
          start_rains <- annual_rainfall_start_rains(definitions_season, daily, data_names)
          summary_data <- join_null_data(summary_data, start_rains)
          summary_data$start_rains <- as.integer(summary_data$start_rains_doy)
          summaries <- c(summaries, "start_rains")
        } else {
          stop("Cannot calculate seasonal_length without start_rains in definitions file.")
        }
        require_start_rains <- TRUE
      }
      if (!require_end_rains){ #  if end_ is not given then ...
        # check for start_rains in definitions file
        if ("end_season" %in% names(definitions_season)){
          warning("Creating end_season column to calculate seasonal summaries")
          if (!is.null(definitions$start_rains$s_start_doy)) definitions$end_season$s_start_doy <- definitions$start_rains$s_start_doy
          end_season <- annual_rainfall_end_season(definitions_season, daily, data_names)
          summary_data <- join_null_data(summary_data, end_season)
          summary_data$end_season <- as.integer(summary_data$end_season_doy)
          summaries <- c(summaries, "end_season")
        } else if ("end_rains" %in% names(definitions_season)){
          warning("Creating end_rains column to calculate seasonal summaries")
          if (!is.null(definitions$start_rains$s_start_doy)) definitions$end_rains$s_start_doy <- definitions$start_rains$s_start_doy
          end_rains <- annual_rainfall_end_rains(definitions_season, daily, data_names)
          summary_data <- join_null_data(summary_data, end_rains)
          summary_data$end_rains <- as.integer(summary_data$end_rains_doy)
          summaries <- c(summaries, "end_rains")
        } else {
          stop("Cannot calculate seasonal_length without end_rains or end_season in definitions file.")
        }
        require_end_rains <- TRUE
      }
      definitions <- c(definitions, definitions_season)
      names_definitions <- unique(names(definitions))
      definitions <- unique(definitions)
      names(definitions) <- names_definitions
      season_rain <- annual_rainfall_seasonal_length(definitions, daily, summary_data, data_names, summaries)
      summary_data <- join_null_data(summary_data, season_rain)
    }
  }
  
  if (!is.null(definitions$start_rains$s_start_doy)){
    summary_data$year <- as.integer(sub("-.*", "", summary_data$year))
  }
  
  if ("annual_rain" %in% summaries) {
    annual_rain <- annual_rainfall_annual_rain(definitions, daily, data_names)
    summary_data <- join_null_data(summary_data, annual_rain)
  }
  list_return <- NULL
  
  # rename
  list_return[[1]] <- c(definitions)
  list_return[[2]] <- summary_data
  return(list_return)
}
