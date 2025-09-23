#' Annual Rainfall Summaries
#' 
#' @description A table containing all the annual rainfall summaries for PICSA
#' e.g. start of rain, total rainfall, number of rain days, end of season.
#' One row per year/station and one column per summary.
#'
#' @param country `character(1)` The country code of the data.
#' @param station_id `character` The id's of the stations to analyse. Either a
#'   single value or a vector.
#' @param call A character vector specifying where to call the raw data from if calling raw data.
#' @param summaries `character` The names of the summaries to produce.
#' @param override A logical argument default `FALSE` indicating whether to calculate the summaries still, even if they are stored already in the bucket.
#'
#' @return A data frame with yearly summaries.
#' @export
#' @importFrom rlang :=
#' @examples
#' #annual_rainfall_summaries(country = "zm", station_id = "01122", summaries = "annual_rain")
#' #annual_rainfall_summaries(country = "zm", station_id = "16", summaries = c("start_rains", "end_rains", "annual_rain", "seasonal_rain")) #, "end_season"))
annual_rainfall_summaries <- function(country, station_id, call = c("climsoft", "googlebuckets"),
                                      summaries = c("annual_rain", "start_rains", "end_rains", "end_season", "seasonal_rain", "seasonal_length"), override = FALSE) {
  list_return <- NULL
  call <- match.arg(call)

  # we get the definitions_id from station_id metadata.
  definitions_id <- get_definitions_id_from_metadata(country, station_id)
  
  # do the summaries exist already?
  get_summaries <- get_summaries_data(country, station_id, summary = "annual_rainfall_summaries")
  summary_data <- get_summaries[[1]]
  timestamp <- get_summaries[[2]]

  # what if the definitions is different? Have an override option.
  # if the summary data exists, and if you do not want to override it then:
  if (nrow(summary_data) > 0 & override == FALSE) {

    # to see definitions that exist in the bucket / whether that definition exists under that ID
    file_name <- get_objects_in_bucket(country, definitions_id, timestamp = timestamp)
    if (nrow(file_name) == 0) {
      list_return[[1]] <- (definitions(country, station_id = station_id, summaries = summaries))
    } else {
      
      list_return[[1]] <- (definitions(country, station_id = station_id, summaries = summaries, file = paste0(definitions_id, ".", timestamp)))
    }

    vars_to_pull <- c("station", "year")
    if ("annual_rain" %in% summaries) vars_to_pull <- c(vars_to_pull, "annual_rain", "n_rain")
    if ("start_rains" %in% summaries) vars_to_pull <- c(vars_to_pull, "start_rains_doy", "start_rains_date")
    if ("end_rains" %in% summaries) vars_to_pull <- c(vars_to_pull, "end_rains_doy", "end_rains_date")
    if ("end_season" %in% summaries) vars_to_pull <- c(vars_to_pull, "end_season_doy", "end_season_date")
    if ("seasonal_rain" %in% summaries) vars_to_pull <- c(vars_to_pull, "start_rains_doy", "start_rains_date", "end_rains_doy", "end_rains_date", "end_season_doy", "end_season_date", "seasonal_rain")
    if ("seasonal_length" %in% summaries) vars_to_pull <- c(vars_to_pull, "start_rains_doy", "start_rains_date", "end_rains_doy", "end_rains_date", "end_season_doy", "end_season_date", "season_length")
    vars_to_pull <- unique(vars_to_pull)
    # set vars_to_pull to only be names in the summary_data
    vars_to_pull <- vars_to_pull[vars_to_pull %in% colnames(summary_data)]
    summary_data <- summary_data %>% dplyr::select(dplyr::all_of(vars_to_pull))
  } else {
    file_name <- get_objects_in_bucket(country, definitions_id, timestamp = timestamp)
    if (nrow(file_name) == 0) {
      definitions <- definitions(country = country, definitions_id = definitions_id, summaries = summaries)
    } else {
      # Get data definitions and summary definitions
      if (!is.null(timestamp)){
        file <- paste0(definitions_id, ".", timestamp)
      } else {
        file <- regmatches(file_name$name[length(file_name$name)], regexpr("(?<=/)[^/]+(?=\\.json)", file_name$name[length(file_name$name)], perl=TRUE))
        }
      definitions <- definitions(country = country, definitions_id = definitions_id, summaries = summaries, file = file)
    }
    definitions_season <- NULL
    # Check if all elements in summaries are present in definitions
    # what about start, ends, and that seasonal_length doesn't need to be defined?
    # if (!all(summaries %in% names(definitions))) {
    #   warning("Not all elements in summaries are present in the definitions file. Looking only at summaries in the definitions file.")
    #   # Reduce summaries to only contain elements present in both summaries and y
    #   summaries <- intersect(summaries, definitions)
    # }
    
    # checking if additional definitions need to be added for the seasonal_* summaries
    for (i in c("seasonal_rain", "seasonal_length")){
      
      # if this is given in the summaries
      if (i %in% summaries){
        
        # if seasonal_* is given in the summaries list, then check we calculate it
        def_end_type <- definitions[[i]]$end_type
        # if an end type is specified for seasonal_*, then make sure we calculate that end type in summaries
        if (!is.null(def_end_type) && (!def_end_type %in% summaries)){
          summaries <- c(summaries, paste0("end_", def_end_type))
          
          # checking we have a definitions file
          if (nrow(file_name) == 0) {
            definitions_2 <- definitions(country = country, definitions_id = definitions_id, summaries = paste0("end_", def_end_type))
          } else {
            definitions_2 <- definitions(country = country, definitions_id = definitions_id, summaries = paste0("end_", def_end_type), file = file)
          }
          
          # if there's no definitions file, then set the end type to be the other one and check for definitions file
          if (length(definitions_2) == 0){ 
            warning(paste0("Ignoring end_type = ", def_end_type, " in seasonal_rain because no definitions given."))
            if (def_end_type == "rains"){
              definitions[[i]]$end_type <- "season"
              definitions_2 <- definitions(country = country, definitions_id = definitions_id, summaries = paste0("end_season"))
              if (length(definitions_2) == 0) stop(paste0("Cannot calculate", i, "without end_rains or end_season in definitions file."))
            } else {
              definitions[[i]]$end_type <- "rains"
              definitions_2 <- definitions(country = country, definitions_id = definitions_id, summaries = paste0("end_rains"))
              if (length(definitions_2) == 0) stop(paste0("Cannot calculate", i, "without end_rains or end_season in definitions file."))
            }
          } else {
            definitions <- c(definitions, definitions_2)
          }
        } 
        
        # if no end type is specified, then...
        if (is.null(def_end_type)){
          # is either end_* in the summaries?
          if (any(grepl("end_", summaries))){
            if (any(grepl("end_season", summaries))){
              definitions[[i]]$end_type <- "season"
            } else {
              definitions[[i]]$end_type <- "rains"
            }
            #great, we have at least one given so we're happy.
            # set end_type as season by default
          } else { # if no end type is specified, and no summaries are specified
            # specified in the code asked for (if both, then end_seasons as per Roger's recommendation)
            definitions_season <- definitions(country = country, definitions_id = definitions_id, summaries = c("end_rains", "end_season"))
            if ("end_season" %in% names(definitions_season)){
              definitions[[i]]$end_type <- "season"
            } else if ("end_rains" %in% names(definitions_season)){
              definitions[[i]]$end_type <- "rains"
            } else {
              stop("Cannot calculate seasonal_rain without end_rains or end_season in definitions file.")
            }
          }
        }
      }
    }
    # Fetch daily data and preprocess
    daily <- get_daily_data(country = country, station_id = station_id, call_from = call)
    
    # For the variable names to be set as a certain default, set TRUE here, and run check_and_rename_variables
    data_names <- data_definitions(names(daily), TRUE)
    daily <- check_and_rename_variables(daily, data_names)
    if (class(daily$date) != "Date") daily$date <- as.Date(daily$date)
    if (!"year" %in% names(daily)) daily$year <- lubridate::year(daily$date)
    
    # Check if start_rains and end_rains are required for seasonal_rain and seasonal_length
    if (any(grepl("seasonal_", summaries))){
      if (!"start_rains" %in% summaries){
        summaries <- c(summaries, "start_rains")
        
        # checking we have a definitions file
        definitions_2 <- definitions(country = country, definitions_id = definitions_id, summaries = "start_rains")
        
        # if there's no definitions file, throw error
        if (length(definitions_2) == 0){ 
          stop(paste0("Cannot calculate seasonal summaries without start_rains in definitions file."))
        } else {
          definitions <- c(definitions, definitions_2)
        }
      }
    }
    require_end_rains <- any(grepl("seasonal_", summaries)) & (any(grepl("end_", summaries)))
    # run the checks to create this above, so we should never have this as false

    summary_data <- NULL
    
    # Calculate summaries ==================================================================
    if ("start_rains" %in% summaries) {
      start_rains <- annual_rainfall_start_rains(definitions, daily, data_names)
      summary_data <- join_null_data(summary_data, start_rains)
      summary_data$start_rains_doy <- as.integer(summary_data$start_rains_doy)
    }

    if ("end_rains" %in% summaries) {
      if (!is.null(definitions$start_rains$s_start_doy) & is.null(definitions$end_rains$s_start_doy)) definitions$end_rains$s_start_doy <- definitions$start_rains$s_start_doy
      end_rains <- annual_rainfall_end_rains(definitions, daily, data_names)
      summary_data <- join_null_data(summary_data, end_rains)
      summary_data$end_rains_doy <- as.integer(summary_data$end_rains_doy)
    }
    
    if ("end_season" %in% summaries) {
      if (!is.null(definitions$start_rains$s_start_doy) & is.null(definitions$end_season$s_start_doy)) definitions$end_season$s_start_doy <- definitions$start_rains$s_start_doy
      end_season <- annual_rainfall_end_season(definitions, daily, data_names)
      summary_data <- join_null_data(summary_data, end_season)
      summary_data$end_season_doy <- as.integer(summary_data$end_season_doy)
    } 
    
    if ("seasonal_rain" %in% summaries) {
      season_rain <- annual_rainfall_seasonal_rain(definitions, daily, summary_data, data_names, summaries)
      summary_data <- dplyr::full_join(summary_data, season_rain)
    }
    
    if ("seasonal_length" %in% summaries) {
      season_rain <- annual_rainfall_seasonal_length(definitions, daily, summary_data, data_names, summaries)
      summary_data <- dplyr::full_join(summary_data, season_rain)
    }
    
    if (!is.null(definitions$start_rains$s_start_doy) | !is.null(definitions$end_season$s_start_doy) | !is.null(definitions$end_rains$s_start_doy)){
      summary_data$year <- factor(sub("-.*", "", summary_data$year))
    }
    
    if ("annual_rain" %in% summaries) {
      if (!is.null(definitions$start_rains$s_start_doy) & is.null(definitions$annual_rain$s_start_doy)) definitions$annual_rain$s_start_doy <- definitions$start_rains$s_start_doy
      annual_rain <- annual_rainfall_annual_rain(definitions, daily, data_names)
      annual_rain$year <- factor(annual_rain$year)
      summary_data <- join_null_data(summary_data, annual_rain)
    }

    names_definitions <- unique(names(definitions))
    definitions <- unique(definitions)
    names(definitions) <- names_definitions
    
    summary_data <- summary_data %>%
      dplyr::mutate(dplyr::across(dplyr::ends_with("_date"), ~as.character(.))) %>%
      dplyr::filter(year %in% unique(daily[[data_names$year]]))
    list_return[[1]] <- definitions
  }
  # rename
  list_return[[2]] <- summary_data
  return(list_return)
}

#' Annual Temperature Summaries
#'
#' @param country `character(1)` The country code of the data.
#' @param station_id `character` The id's of the stations to analyse. Either a
#'   single value or a vector.
#' @param call A character vector specifying where to call the raw data from if calling raw data.
#' @param summaries `character` The names of the summaries to produce.
#' @param override A logical argument default `FALSE` indicating whether to calculate the summaries still, even if they are stored already in the bucket.
#' 
#' @return A data frame with yearly summaries.
#' @export
#'
#' @examples
#' # annual_temperature_summaries(country = "zm", station_id = "16") # made a fake "16" json definitions data
#' # because it contains temperature data. 
annual_temperature_summaries <- function(country,
                                         station_id,
                                         call = c("climsoft", "googlebuckets"),
                                         summaries = c("mean_tmin","mean_tmax", "min_tmin", "min_tmax", "max_tmin", "max_tmax"),
                                         override = FALSE) {
 return(total_temperature_summaries(country = country, station_id = station_id, call = call, summaries = summaries, to = "annual", override = override))
}

#' Probability Crop Tables
#' @description The probabilities of crop success for given planting maturity lengths, seasonal total rainfall requirements, and planting dates.
#'
#' @param country `character(1)` The country code of the data.
#' @param station_id `character` The id's of the stations to analyse. Either a
#'   single value or a vector.
#' @param call A character vector specifying where to call the raw data from if calling raw data.
#' @param water_requirements \code{numeric} Vector containing water requirements requirements.
#' @param planting_dates \code{numeric} Vector containing planting dates requirements.
#' @param planting_length \code{numeric} Vector containing seasonal crop length requirements.
#' @param start_before_season \code{logical} A logical value indicating whether to check the start day condition (default is `TRUE`).
#' @param override A logical argument default `FALSE` indicating whether to calculate the summaries still, even if they are stored already in the bucket.
#'
#' @return A list containing the definitions and a data frame with probability summaries.
#' @export
#'
#' @examples
#' #library(epicsawrap)
#' #library(tidyverse)
#' #epicsawrap::setup(dir = getwd())
#' #epicsawrap::gcs_auth_file(file = "C:/Users/lclem/Downloads/e-picsa-e630400792e7.json")
#' #crop_success_probabilities(country = "zm", station_id = "16")
#' 
#' # or some can be defined in the dialog
#' #x <- crop_success_probabilities(country = "zm", station_id = "16", water_requirements = c(100, 300, 800))
#' 
#' # or all can be defined in the dialog
#' #crop_success_probabilities(country = "zm", station_id = "16", water_requirements = c(100, 300, 800),
#' #                           planting_length = c(100, 150), planting_dates = c(90, 100, 110))
crop_success_probabilities <- function(country,
                                       station_id,
                                       call = c("climsoft", "googlebuckets"),
                                       planting_dates = NULL,
                                       water_requirements = NULL,
                                       planting_length = NULL,
                                       start_before_season = NULL,
                                       override = FALSE) {
  list_return <- NULL
  
  check_and_set_parameter <- function(parameter_name, target_column_name) {
    if (is.null(get(parameter_name))) {
      value <- as.integer(definitions$crops_success[[target_column_name]])
      if (length(value) == 0) stop(paste(parameter_name, "parameter missing in definitions file."))
      assign(parameter_name, value, envir = parent.frame())
    } else {
      definitions$crops_success[[target_column_name]] <- get(parameter_name)
    }
  }
  
  # get definitions_id from station_id metadata.
  definitions_id <- get_definitions_id_from_metadata(country, station_id)
  
  # do the summaries exist already?
  get_summaries <- get_summaries_data(country, station_id, summary = "crop_success_probabilities")
  summary_data <- get_summaries[[1]]
  timestamp <- get_summaries[[2]]
  
  if ((!is.null(planting_dates) | !is.null(water_requirements) | !is.null(planting_length) | !is.null(start_before_season)) & override == FALSE){
    warning("Setting override = TRUE. Access to raw data is required")
    override <- TRUE
    file_id <- paste0(definitions_id, ".", timestamp)
  } else {
    file_id <- definitions_id
  }
  
  # what if the definitions is different? Have an override option.
  # if the summary data exists, and if you do not want to override it then:
  if (nrow(summary_data) > 0 & override == FALSE) {
    file_name <- get_objects_in_bucket(country, definitions_id, timestamp = timestamp)
    if (nrow(file_name) == 0) {
      definitions <- definitions(country, station_id = station_id, summaries = "crops_success")
    } else {
      definitions <- definitions(country, station_id = station_id, summaries = "crops_success", file = paste0(definitions_id, ".", timestamp))
    }
    definitions$crops_success$planting_length <- check_and_set_parameter("planting_length", "planting_length")
    definitions$crops_success$water_requirements <- check_and_set_parameter("water_requirements", "water_requirements")
    definitions$crops_success$planting_dates <- check_and_set_parameter("planting_dates", "planting_dates")
    list_return[[1]] <- definitions
    
  } else {
    # check bucket for file
    file_name <- get_objects_in_bucket(country, definitions_id, timestamp = timestamp)
    if (nrow(file_name) == 0) {
      definitions <- definitions(country = country, definitions_id = definitions_id, summaries = "crops_success")
    } else {
      # Get data definitions and summary definitions
      if (!is.null(timestamp)){
        file <- paste0(definitions_id, ".", timestamp)
      } else {
        file <- regmatches(file_name$name[length(file_name$name)], regexpr("(?<=/)[^/]+(?=\\.json)", file_name$name[length(file_name$name)], perl=TRUE))
      }
      definitions <- definitions(country = country, definitions_id = definitions_id, summaries = "crops_success", file = file)
    }
    
    if (override){
          # if we are overriding, then we are overriding for our start_rains definition too, meaning we need to recalculate that
    # Fetch daily data and preprocess
    daily <- get_daily_data(country = country, station_id = station_id, call_from = call)
    
    # For the variable names to be set as a certain default, set TRUE here, and run check_and_rename_variables
    data_names <- data_definitions(names(daily), TRUE)
    daily <- check_and_rename_variables(daily, data_names)
    
    season_data <- annual_rainfall_summaries(country = country, station_id = station_id, call = call, summaries = c("start_rains", "end_rains"), override = override) # end rains or end season?
    #offset <- season_data[[1]]$start_rains$s_start_doy
    
    definitions$crops_success$planting_length <- check_and_set_parameter("planting_length", "planting_length")
    definitions$crops_success$water_requirements <- check_and_set_parameter("water_requirements", "water_requirements")
    definitions$crops_success$planting_dates <- check_and_set_parameter("planting_dates", "planting_dates")
  
    if (is.null(start_before_season)){
      start_before_season <- is.logical(definitions$crops_success$start_check)
      if (length(start_before_season) == 0) stop("start_before_season parameter missing in definitions file.")
    } else {
      definitions$crops_success$start_check <- start_before_season
    }
    
    daily$year <- factor(daily$year)
    summary_data <- rpicsa::crops_definitions(data = daily,
                                              date_time  = data_names$date,
                                              station = data_names$station,
                                              year = data_names$year,
                                              rain = data_names$rain,
                                              water_requirements = as.integer(water_requirements),
                                              planting_dates = as.integer(planting_dates),
                                              planting_length = as.integer(planting_length),
                                              start_check = start_before_season,
                                              season_data = season_data[[2]],
                                              start_day = "start_rains_doy",
                                              end_day = "end_rains_doy")
    list_return[[1]] <- c(season_data[[1]], definitions)
  }

  }
  # rename
  list_return[[2]] <- summary_data
  return(list_return)
}

#' Get the Extreme Data
#'
#' This function identifies extreme values in a specified element (column) of a data frame. It can operate in two modes: percentile-based and threshold-based.
#' 
#' @param country A character string specifying the country code of the data.
#' @param station_id A character vector specifying the ID(s) of the station(s) to analyse.
#' @param summaries A character vector specifying the names of the summaries to produce.
#' @param override A logical argument default `FALSE` indicating whether to calculate the summaries still, even if they are stored already in the bucket.
#'
#' @return A data frame containing the extreme data.
#' @export
#' 
#' @examples
#' # Generate annual temperature summaries for station 16 in Zambia
#' #extremes_summaries(country, station_id, c("extremes_rain"))
extremes_summaries <- function(country, station_id,
                               summaries = c("extremes_rain", "extremes_tmin", "extremes_tmax"),
                               override = FALSE){
  list_return <- NULL
  
  # do the summaries exist already?
  get_summaries <- get_summaries_data(country, station_id, summary = "extremes_summaries")
  summary_data <- get_summaries[[1]]
  
  # if the summary data exists, and if you do not want to override it then:
  if (nrow(summary_data) > 0 & override == FALSE) {
    # what if the definitions is different? 
    file_name <- get_objects_in_bucket(country, station_id, timestamp = get_summaries[[2]])
    if (nrow(file_name) == 0) {
      list_return[[1]] <- (definitions(country, station_id = station_id, summaries = summaries))
    } else {
      list_return[[1]] <- (definitions(country, station_id = station_id, summaries = summaries, file = paste0(station_id, ".", get_summaries[[2]])))
    }
  } else {
    # Fetch daily data and preprocess
    daily <- get_daily_data(country = country, station_id = station_id)
    # For the variable names to be set as a certain default, set TRUE here, and run check_and_rename_variables
    data_names <- data_definitions(names(daily), TRUE)
    daily <- check_and_rename_variables(daily, data_names)
    definitions <- epicsawrap::definitions(country = country, station_id = station_id, summaries = summaries)
    summary_data <- purrr::map(.x = summaries, .f = ~ overall_extremes_summaries(daily = daily, data_names = data_names, definitions = definitions, summaries = .x))
    summary_data <- purrr::reduce(summary_data, dplyr::full_join)
    summary_data[is.na(summary_data)] <- 0
    list_return[[1]] <- c(definitions)
  }
  list_return[[2]] <- summary_data
  return(list_return)
}

#' Monthly Temperature Summaries
#'
#' @param country `character(1)` The country code of the data.
#' @param station_id `character` The id's of the stations to analyse. Either a
#'   single value or a vector.
#' @param call A character vector specifying where to call the raw data from if calling raw data.
#' @param summaries `character` The names of the summaries to produce.
#' @param override A logical argument default `FALSE` indicating whether to calculate the summaries still, even if they are stored already in the bucket.
#' 
#' @return A data frame with monthly summaries.
monthly_temperature_summaries <- function(country,
                                         station_id,
                                         call = c("climsoft", "googlebuckets"),
                                         summaries = c("mean_tmin","mean_tmax", "min_tmin", "min_tmax", "max_tmin", "max_tmax"), override = FALSE) {
 return(total_temperature_summaries(country = country, station_id = station_id, call = call, summaries = summaries, to = "monthly", override = override))
}

#' Season start date probabilities
#' @description A table containing the probabilities of the season starting on or before a set of particular dates.
#'
#' @param country `character(1)` The country code of the data.
#' @param station_id `character` The id's of the stations to analyse. Either a
#'   single value or a vector.
#' @param call A character vector specifying where to call the raw data from if calling raw data.
#' @param start_dates `numeric` A vector of start dates (in doy format) to calculate the probabilities of the season starting on or before.
#' @param override A logical argument default `FALSE` indicating whether to calculate the summaries still, even if they are stored already in the bucket.
#'
#' @return A list containing the definitions and a data frame with probability summaries.
#' @export
#'
#' @examples #
#' #library(epicsawrap)
#' #library(tidyverse)
#' #epicsawrap::setup(dir = getwd())
#' #epicsawrap::gcs_auth_file(file = "C:/Users/lclem/Downloads/e-picsa-e630400792e7.json")
#' #season_start_probabilities(country = "zm", station_id = "16")
#' # or you can manually define
#' #season_start_probabilities(country = "zm", station_id = "16", start_dates = c(10, 20, 100))
season_start_probabilities <- function(country,
                                       station_id,
                                       call = c("climsoft", "googlebuckets"),
                                       start_dates = NULL,
                                       override = FALSE) {
  list_return <- NULL
  
  # get definitions_id from station_id metadata.
  definitions_id <- get_definitions_id_from_metadata(country, station_id)
  summaries <- "season_start_probabilities"
  
  # do the summaries exist already?
  get_summaries <- get_summaries_data(country, station_id, summary = summaries)
  summary_data <- get_summaries[[1]]
  timestamp <- get_summaries[[2]]
  
  # what if the definitions is different? Have an override option.
  
  #if the summary data exists, and if you do not want to override it then:
  if (!is.null(start_dates) & override == FALSE & nrow(summary_data) > 0) warning("Override set to TRUE for calculating start dates. Using saved data for start_rains")
  
  if (nrow(summary_data) > 0 & override == FALSE & is.null(start_dates)) {
    file_name <- get_objects_in_bucket(country, definitions_id, timestamp = timestamp)
    if (nrow(file_name) == 0) {
      list_return[[1]] <- (definitions(country, station_id = station_id, summaries = summaries))
    } else {
      list_return[[1]] <- (definitions(country, station_id = station_id, summaries = summaries, file = paste0(definitions_id, ".", timestamp)))
    }
  } else {
    file_name <- get_objects_in_bucket(country, definitions_id, timestamp = timestamp)
    if (nrow(file_name) == 0) {
      definitions <- definitions(country = country, definitions_id = definitions_id, summaries = summaries)
    } else {
      # Get data definitions and summary definitions
      if (!is.null(timestamp)){
        file <- paste0(definitions_id, ".", timestamp)
      } else {
        file <- regmatches(file_name$name[length(file_name$name)], regexpr("(?<=/)[^/]+(?=\\.json)", file_name$name[length(file_name$name)], perl=TRUE))
      }
      definitions <- definitions(country = country, definitions_id = definitions_id, summaries = summaries, file = file)
    }
    
    # if we are overriding, then we are overriding for our start_rains definition too, meaning we need to recalculate that
    if (override){
      # Fetch daily data and preprocess
      daily <- get_daily_data(country = country, station_id = station_id, call_from = call)
      # For the variable names to be set as a certain default, set TRUE here, and run check_and_rename_variables
      data_names <- data_definitions(names(daily), TRUE)
      daily <- check_and_rename_variables(daily, data_names)
      if (class(daily$date) != "Date") daily$date <- as.Date(daily$date)
      if (!"year" %in% names(daily)) daily$year <- lubridate::year(daily$date)
      
          season_data <- annual_rainfall_summaries(country = country, station_id = station_id, call = call, summaries = c("start_rains"), override = override)
    if (is.null(start_dates)){
      start_dates <- definitions$season_start_probabilities$specified_day
      if (length(start_dates) == 0) stop("start_dates parameter missing in definitions file.")
    } else {
      definitions$season_start_probabilities$specified_day <- start_dates
    }
          #summary_data <- rpicsa::probability_season_start(data = season_data[[2]],
          #                                           station = data_names$station,
          #                                           start_rains = "start_rains_doy",
          #                                           doy_format = "doy_366", # we calculate this in the start_rains summaries?
          #                                           specified_day = as.integer(start_dates))
    list_return[[1]] <- c(season_data[[1]], definitions)
    } else {
      data_names <- NULL
      data_names$station <- "station"
    }
  }
  list_return[[2]] <- summary_data
  return(list_return)
}

#' Generate Annual and Monthly Temperature Summaries
#'
#' This function calculates annual or monthly temperature summaries for specified stations in a given country.
#'
#' @param country A character string specifying the country code of the data.
#' @param station_id A character vector specifying the ID(s) of the station(s) to analyse.
#' @param call A character vector specifying where to call the raw data from if calling raw data.
#' @param summaries A character vector specifying the names of the summaries to produce.
#' @param to A character string indicating whether the summaries should be generated for "annual" or "monthly" data.
#' @param override A logical argument default `FALSE` indicating whether to calculate the summaries still, even if they are stored already in the bucket.
#'
#' @return A data frame containing the requested temperature summaries.
#'
#' @examples
#' # Generate annual temperature summaries for station 16 in Zambia
#' #total_temperature_summaries(country = "zm", station_id = "1", summaries = c("mean_tmin", "mean_tmax", "min_tmin", "max_tmax"), to = "annual")
total_temperature_summaries <- function(country,
                                        station_id,
                                        call = c("climsoft", "googlebuckets"),
                                        summaries = c("mean_tmin", "mean_tmax", "min_tmin", "min_tmax", "max_tmin", "max_tmax"),
                                        to = c("annual", "monthly"),
                                        override = FALSE) {
  to <- match.arg(to)
  call <- match.arg(call)
  list_return <- NULL
  
  # we get the definitions_id from station_id metadata.
  definitions_id <- get_definitions_id_from_metadata(country, station_id)
  
  # do the summaries exist already?
  get_summaries <- get_summaries_data(country, station_id, summary = paste0(to, "_temperature_summaries"))
  summary_data <- get_summaries[[1]]
  timestamp <- get_summaries[[2]]
  
  # what if the definitions is different? Have an override option.
  # if the summary data exists, and if you do not want to override it then:
  if (nrow(summary_data) > 0 & override == FALSE) {
    # to see definitions that exist in the bucket / whether that definition exists under that ID
    file_name <- get_objects_in_bucket(country, definitions_id, timestamp = timestamp)
    if (nrow(file_name) == 0) {
      list_return[[1]] <- (definitions(country, station_id = station_id, summaries = summaries))
    } else {
      list_return[[1]] <- (definitions(country, station_id = station_id, summaries = summaries, file = paste0(definitions_id, ".", timestamp)))
    }
    
    # set vars_to_pull to only be names in the summary_data
    vars_to_pull <- c("station", "year", "month", summaries)
    vars_to_pull <- vars_to_pull[vars_to_pull %in% colnames(summary_data)]
    summary_data <- summary_data %>% dplyr::select(dplyr::all_of(vars_to_pull))
    
  } else {
    file_name <- get_objects_in_bucket(country, definitions_id, timestamp = timestamp)
    if (nrow(file_name) == 0) {
      definitions <- definitions(country = country, definitions_id = definitions_id, summaries = summaries)
    } else {
      # Get data definitions and summary definitions
      if (!is.null(timestamp)){
        file <- paste0(definitions_id, ".", timestamp)
      } else {
        file <- regmatches(file_name$name[length(file_name$name)], regexpr("(?<=/)[^/]+(?=\\.json)", file_name$name[length(file_name$name)], perl=TRUE))
      }
      definitions <- definitions(country = country, definitions_id = definitions_id, summaries = summaries, file = file)
    }
    
    # Fetch daily data and preprocess
    daily <- get_daily_data(country = country, station_id = station_id, call_from = call)

    # For the variable names to be set as a certain default, set TRUE here, and run check_and_rename_variables
    data_names <- data_definitions(names(daily), TRUE)
    daily <- check_and_rename_variables(daily, data_names)
    if (class(daily$date) != "Date") daily$date <- as.Date(daily$date)
    if (!"year" %in% names(daily)) daily$year <- lubridate::year(daily$date)
    summary_data <- NULL

    for (summary in summaries) {
      if (is.null(definitions[[summary]]$to)) {
        stop(paste0("'", summary, "' has been given in summaries but no data is given in definitions json file."))
      } else {
        if (any(grepl(to, x = unlist(definitions[[summary]]$to)))){
          if (is.null(definitions[[summary]]$na_rm)){
            warning("Missing parameter value for na_rm in ", summary, ". Setting as FALSE.")
            definitions$annual_rain$na_rm <- FALSE
          }
          summary_type <- gsub("_.*$", "", summary)
          summary_variable <- gsub("^.*_", "", summary)
          if (length(as.logical(definitions[[summary]]$na_rm)) == 0) definitions[[summary]]$na_rm <- FALSE
          summary_data[[summary]] <- rpicsa::summary_temperature(data = daily,
                                                                 date_time = data_names$date,
                                                                 station = data_names$station,
                                                                 year = data_names$year,
                                                                 tmax = if (summary_variable == "tmax") data_names$tmax else NULL,
                                                                 tmin = if (summary_variable == "tmin") data_names$tmin else NULL,
                                                                 summaries = summary_type,
                                                                 to = to,
                                                                 na_rm = as.logical(definitions[[summary]]$na_rm),
                                                                 na_prop = definitions[[summary]]$na_prop,
                                                                 na_n = definitions[[summary]]$na_n,
                                                                 na_consec = definitions[[summary]]$na_consec,
                                                                 na_n_non = definitions[[summary]]$na_n_non)
        }
      }
    }
    if (length(summary_data) > 1){
      summary_data <- Reduce(function(x, y) dplyr::full_join(x, y), summary_data)
    } else {
      summary_data <- summary_data[[1]]
    }
    list_return[[1]] <- definitions
  }
  summary_data$year <- as.integer(summary_data$year)
  if ("month" %in% names(summary_data)){
    summary_data$month <- as.integer(forcats::as_factor(summary_data$month))
  }
  
  # rename
  list_return[[2]] <- summary_data
  return(list_return)
}

#' Generate summary statistics for extreme weather events.
#'
#' This function generates summary statistics for extreme weather events based on given definitions.
#'
#' @param daily A dataframe containing daily weather data.
#' @param data_names A list containing the data names in the `daily` data.
#' @param definitions A list containing definitions of extreme events.
#' @param summaries Name of the summary to be generated.
#'
#' @return A dataframe containing summary statistics for extreme events.
#'
#' @examples
#' # Example usage:
#' # Generate summary statistics for extreme rain events
#' #rain_summary <- overall_extremes_summaries(daily_data, definitions_list, "extremes_rain")
overall_extremes_summaries <- function(daily, data_names, definitions, summaries){
  if (is.null(definitions[[summaries]]$direction)){
    direction <- "greater"
  } else {
    direction <- as.character(definitions[[summaries]]$direction)
  }
  if (is.null(definitions[[summaries]]$type)){
    type <- "greater"
  } else {
    type <- as.character(definitions[[summaries]]$type)
  }
  
  summary_data <- rpicsa::get_extremes(data = daily,
                                       station = data_names$station,
                                       year = data_names$year,
                                       element = if (summaries == "extremes_rain") data_names$rain else if (summaries == "extremes_tmin") data_names$tmin else if (summaries == "extremes_tmax") data_names$tmax else NULL,
                                       type = type,
                                       direction = direction,
                                       value = as.integer(definitions[[summaries]]$value))
  summary_data <- summary_data %>% dplyr::rename(!!summaries := "count")
  return(summary_data)
}

#' Get Daily Data
#'
#' @param country A character vector specifying the country or countries from which to get the data. Common options are `"mz"`, `"zm"`, and `"zm_test"`. Any defined in `get_bucket_name()`.
#' @param station_id A character string specifying the ID of the station for which to get the daily data.
#' @param call_from A character string specifying the location of the raw data.
#'
#' @return A data frame containing the daily data for the specified station and country.
#' @export
#'
#' @examples #
get_daily_data <- function(country, station_id, call_from = c("climsoft", "googlebuckets")) {
  call_from <- match.arg(call_from)
  if (length(country) > 1) stop("'country' must be of length 1")
  station_id <- as.character(station_id)
  
  if (call_from == "climsoft"){
    # if you call from climsoft
    climsoft_info <- get_station_metadata(country = country, station_id = station_id)$climsoft_list
    if (is.null(get_climsoft_conn())) stop("Set climsoft connection with set_climsoft_conn() function.")
    station_data <- import_from_climsoft(con = get_climsoft_conn(),
                                         stations = station_id,
                                         include_station_info = FALSE,
                                         elementfiltercolumn = climsoft_info[[1]]$elementfiltercolumn,
                                         elements = climsoft_info[[1]]$elements)
  } else {
    # if you call from googlebuckets
    dfs <- vector("list", length(station_id))
    names(dfs) <- station_id
    for (i in seq_along(station_id)) {
      f <- paste0(country, "/", "data", "/", station_id[i], ".rds")
      if (file.exists(f)) {
        dfs[[i]] <- readRDS(f)
      } else {
        f <- update_daily_data(country, station_id[i])
        dfs[[i]] <- f#saveRDS(o, file = f)
      }
    }
    if (length(station_id) > 1) {
      station_data <- dplyr::bind_rows(dfs)
    } else station_data <- dfs[[1]] 
  }
  return(station_data)
}

#' Get Definitions ID from Metadata
#'
#' This function retrieves the definitions ID from station metadata for a given country and station ID.
#'
#' @param country A character string representing the country code.
#' @param station_id A character string representing the station ID.
#'
#' @return A character string representing the definitions ID from the station metadata.
get_definitions_id_from_metadata <- function(country, station_id) {
  station_id_metadata <- get_station_metadata(country = country, station_id = station_id, include_definitions = FALSE)
  if (nrow(station_id_metadata) == 0) {
    warning(paste0(station_id, " not found in metadata. No definition ID given. Returning station_id."))
    definitions_id <- station_id
  } else {
    id_store <- station_id_metadata$definitions_id
    definitions_id <- id_store[[1]][length(id_store[[1]])] # currently get the most recent ID 
  }
  return(definitions_id)
}

#' Import Data from Climsoft
#'
#' Connects to a Climsoft database and imports data based on the specified filters for stations and elements, with options to include observation flags and station information.
#'
#' @param con Connection object to the Climsoft database, default is the result of \code{get_climsoft_conn()}.
#' @param stationfiltercolumn Name of the column to filter by stations, default is 'stationId'.
#' @param stations Vector of station IDs to filter the data, defaults to an empty vector.
#' @param elementfiltercolumn Name of the column to filter by elements, default is 'elementId'.
#' @param elements Vector of element IDs to filter the data, defaults to an empty vector.
#' @param include_observation_flags Boolean, if TRUE includes observation flags in the output, defaults to FALSE.
#' @param include_station_info Boolean, if TRUE includes station metadata in the output, defaults to FALSE.
#' @param unstack_data Boolean. Option to unstack data once read in. 
#' @param start_date Start date for filtering the observations, format should be Date, defaults to NULL.
#' @param end_date End date for filtering the observations, format should be Date, defaults to NULL.
#' 
#' @return A list containing Climsoft station and observation data based on the filters applied. If `include_station_info` is TRUE, the list will have two elements: 'Metadata' with station details and 'Daily data' with observations.
#' 
#' @examples
#' #con <- get_climsoft_conn()
#' #data <- import_from_climsoft(con, stations = c("101", "102"), elements = c("1", "2"), start_date = as.Date("2020-01-01"), end_date = as.Date("2020-01-31"))
#'
#' @export
import_from_climsoft <- function(con = get_climsoft_conn(),
                                 stationfiltercolumn = "stationId",
                                 stations = c(),
                                 elementfiltercolumn = "elementId",
                                 elements = c(),
                                 include_observation_flags = FALSE,
                                 include_station_info = FALSE,
                                 unstack_data = TRUE,
                                 start_date = NULL,
                                 end_date = NULL) {
  con <- con # get connection
  
  #get stations database data and station ids values
  if (length(stations) > 0) {
    #construct a string of station values from the passed station vector eg of result ('191','122')
    passed_station_values <- paste0("(", paste0("'", stations, "'", collapse =  ", "), ")")
    
    #get the station info of the passed station values
    db_station_info <- DBI::dbGetQuery(con, paste0( "SELECT * FROM station WHERE ", stationfiltercolumn, " IN ", passed_station_values,  ";"))
    
    #set values of station ids only
    if (stationfiltercolumn == "stationId") {
      station_ids_values <- passed_station_values
    } else{
      station_ids_values <- paste0("(", paste0("'", db_station_info$stationId, "'", collapse = ", "),")")
    }
  }
  
  #if there are no elements passed then stop and throw error
  if (length(elements) < 1) stop("start_date must be of type Date.")
  
  #set values of element ids only
  if (elementfiltercolumn == "elementId") {
    #get element id values directly from passed data
    element_ids_values <- paste0("(", paste0(elements, collapse = ", "), ")")
  } else{
    #get element id values from the database
    passed_element_values <- paste0("(", paste0("'", elements, "'", collapse = ", "), ")")
    db_elements_ids <- DBI::dbGetQuery( con, paste0("SELECT elementId FROM obselement WHERE ", elementfiltercolumn,  " IN ",  passed_element_values, ";" ))
    element_ids_values <- paste0("(", paste0(sprintf("%d", db_elements_ids$elementId), collapse = ", "), ")")
  }
  
  # if(include_elements_info) {
  #   db_elements_info <- DBI::dbGetQuery(con, paste0("SELECT elementId, elementName, abbreviation, description, elementtype, upperLimit, lowerLimit, units FROM obselement WHERE elementId ", " IN ", element_ids_values, ";" ))
  # }
  
  flags_column_col_sql <- " "
  if (include_observation_flags) {
    flags_column_col_sql <- ", observationfinal.flag AS flag"
  }
  
  #get databounds filter query if dates have been passed
  date_bounds_filter <- ""
  if (!is.null(start_date)) {
    if (!lubridate::is.Date(start_date))
      stop("start_date must be of type Date.")
    start_date <- format(start_date, format = "%Y-%m-%d")
    date_bounds_filter = paste0(date_bounds_filter, " AND obsDatetime >= ", sQuote(start_date))
  }
  if (!is.null(end_date)) {
    if (!lubridate::is.Date(end_date))
      stop("end_date must be of type Date.")
    end_date <- format(end_date, format = "%Y-%m-%d")
    date_bounds_filter <- paste0(date_bounds_filter," AND obsDatetime <=", sQuote(end_date))
  }
  
  #construct observation data sql query and get data from database
  if (length(stations) > 0) {
    #if stations passed get observation data of selected elements of passed stations
    db_observation_data <- DBI::dbGetQuery(con, paste0("SELECT observationfinal.recordedFrom As station, obselement.abbreviation AS element, observationfinal.obsDatetime AS datetime, observationfinal.obsValue AS obsvalue", flags_column_col_sql, " FROM observationfinal INNER JOIN obselement ON observationfinal.describedBy = obselement.elementId WHERE observationfinal.recordedFrom IN ", station_ids_values, " AND observationfinal.describedBy IN ", element_ids_values, date_bounds_filter, " ORDER BY observationfinal.recordedFrom, observationfinal.describedBy;"))
  } else{
    #if stations have not been passed get observation data of passed elements of all stations
    db_observation_data <- DBI::dbGetQuery(con, paste0("SELECT observationfinal.recordedFrom As station, obselement.abbreviation AS element, observationfinal.obsDatetime AS datetime, observationfinal.obsValue AS obsvalue", flags_column_col_sql, " FROM observationfinal INNER JOIN obselement ON observationfinal.describedBy = obselement.elementId WHERE observationfinal.describedBy IN ", element_ids_values, date_bounds_filter, " ORDER BY observationfinal.recordedFrom, observationfinal.describedBy;"))
    
    #then get the stations ids (uniquely) from the observation data and use the ids to get station info
    station_ids_values <- paste0("(", paste0("'", as.character(unique(db_observation_data$station) ), "'", collapse = ", "), ")")
    db_station_info <- DBI::dbGetQuery(con, paste0("SELECT * FROM station WHERE stationId IN ", station_ids_values, ";" ))
  }

  if(unstack_data){
    db_observation_data <- tidyr::pivot_wider(db_observation_data,
                                                     names_from = element,
                                                     values_from = obsvalue)
  }
  if (include_station_info) {
    data_list <- list(db_station_info, db_observation_data)
    names(data_list) <- c("Metadata", "Daily data")
  } else {
    data_list <- db_observation_data
  }
  return(data_list)
}

#' Get the Daily Data
#' 
#' @description This function updates the daily data for a specific station in the specified country. It retrieves the data from Google Cloud Storage using the `get_data` function.
#'
#' @param country A character vector specifying the country or countries from which to update the data. Options are defined in `get_bucket_name()` (e.g., `"zm"`, `"mw"`).
#' @param station_id A character string specifying the ID of the station for which to update the daily data.
#' 
#' @details 
#' The `update_daily_data` function is used to update the daily data for a specific station in the specified country. It internally calls the `get_data` function to retrieve the data from Google Cloud Storage.
#' The `country` argument is a character vector that allows specifying one or more countries from which to update the data. The data will be updated for Mozambique (`"mz"`) and Zambia (`"zm"`). You can modify this argument to update data for different countries.
#' The `station_id` argument is a character string that specifies the ID of the station for which to update the daily data. The function will construct the filename by concatenating the `"data/"` directory, the `station_id`, and the `file` extension `".rds"`. The filename will be passed to the `get_data` function to retrieve the data.
#' The function uses the invisible function to suppress the output of the `get_data` function, ensuring that the data retrieval process is not visible in the console.
#'
#' @return This function does not return any value explicitly. It gets the daily data for the specified station in the specified country.
#' @export
#'
#' @examples # todo
update_daily_data <- function(country, station_id) {
  filename <- paste0("data", "/", station_id, ".rds")
  invisible(get_data(country = country, filename = filename))
}

#' Import Summary Definitions
#' 
#' This function imports summary definitions based on the country, station ID, summaries, and get_summaries.
#' It checks if there are any files corresponding to the provided station and summary in the Google Cloud Storage bucket.
#' If files are found, it imports definitions from the file; otherwise, it generates new definitions.
#' 
#' @param country A character string specifying the country.
#' @param station_id A character string specifying the station ID.
#' @param timestamp The timestamp on the object file name to import. Default `NULL`
#' 
#' @return A list containing imported or generated summary definitions.
#' 
#' @importFrom googleCloudStorageR gcs_list_objects
#' 
#' @examples
#' # Import summary definitions
#' #import_summary_definitions("USA", "station123", list("info1", "info2"))
get_objects_in_bucket <- function(country, station_id, timestamp) {
  if (!is.null(timestamp)){
    file_name <- paste0(station_id, ".", timestamp)
  } else {
    file_name <- station_id
    #         file <- regmatches(file_name$name[length(file_name$name)], regexpr("(?<=/)[^/]+(?=\\.json)", file_name$name[length(file_name$name)], perl=TRUE))
  }
  bucket_name <- get_bucket_name(country)
  files <- googleCloudStorageR::gcs_list_objects(bucket = bucket_name,
                                                 prefix = paste0("definitions/", file_name, "."),
                                                 versions = TRUE)
  return(files)
}

#' Definitions
#'
#' @param country `character(1)` The country code of the data.
#' @param definitions_id `character(1)` The definitions code in the data.
#' @param station_id `character(1)` The definitions code in the data.
#' @param summaries `character` Vector of summaries to display
#' @param file Default `NULL` meaning that the most recent definitions file will be found and imported. Otherwise specify as a string the file to import. In format: "STATIONNAME.TIMESTAMP" e.g. "1.20240311152831"
#'
#' @return TODO
#' @export
#'
#' @examples # e.g. definitions("zm", "16", "annual_rain")
#' # error: definitions("zm", "1", c("annual_rain", "hi", "end_season"))
definitions <- function(country, station_id = NULL, definitions_id = NULL, summaries, file = NULL){
  definition_data <- get_definitions_data(country = country, station_id = station_id, file = file)
  definition_data <- purrr::map(.x = summaries, .f = ~ definition_data[[.x]])
  names(definition_data) <- summaries
  # are any NULL 1 = NULL
  null_check <- purrr::map_dbl(.x = summaries, .f = ~ is.null(definition_data[[.x]]))
  if (any(null_check == 1)){
    warning(paste0("Not all summaries are defined in the json definition file: ", paste(x = names(definition_data)[which(null_check == 1)], collapse = ", ")))
  }
  definition_data[sapply(definition_data, is.null)] <- NULL
  return(definition_data)
}

#' Check and Rename Variables in a Dataset
#'
#' This function checks the variable names in a dataset and renames them based on
#' the provided `data_names` vector.
#'
#' @param data A data frame or data table containing the dataset.
#' @param data_names A named character vector where the names are the standard variable names,
#'        and the values are the corresponding variable names in the dataset.
#'
#' @return A data frame with variable names renamed according to `data_names`.
check_and_rename_variables <- function(data, data_names) {
  # Check if all variable names in data_names exist in the dataset
  missing_vars <- setdiff(names(data_names), names(data))
  missing_names <- setdiff(names(data), names(data_names))
  
  # Rename if variables are missing in the list of data_names
  if (length(missing_vars) > 0) {
    variations <- list(station = c("station_name", "name","station"),
                       date = c("date", "Date"), 
                       year = c("year"),
                       month = c("month_abbr", "month"),
                       doy = c("DOY", "doy_366", "doy"),
                       day = c("day"),
                       rain = c("rain", "rainfall", "precipitation", "PRECIP"),
                       tmax = c("tmax", "max_temperature", "maximum", "max", "temperature_max", "TMPMAX"),
                       tmin = c("tmin","min_temperature", "minimum", "min", "temperature_min", "TMPMIN"))
    
    # Loop through the missing variable names
    
    for (var in missing_vars) {
      variations_pattern <- paste(variations[[var]], collapse = "|")
      
      # see potential matches that could be "station" (or whatever the variable is)
      matches <- missing_names[grepl(variations_pattern, missing_names)]
      
      # now how many are there?
      if (length(matches) < 1){
        warning(paste0("No matches found for ", var))
      } else {
        if (length(matches) > 1) {
          pattern_parts <- unlist(strsplit(variations_pattern, "\\|"))
          matches1 <- matches[matches %in% pattern_parts]
          if (length(matches1) == 0) {
            warning("multiple matches in variable names for ", paste(matches, collapse = ", "), ".\n Setting ", matches[1], " as ", var, ".")
            matches <- matches[1]
          } else {
            matches <- matches1
          }
        }
        colnames(data)[colnames(data) == matches] <- var
        cat("Renamed", matches, "to", var, "\n")
      }
    }
    
  }
  return(data)
}
