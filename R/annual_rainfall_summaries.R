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
  get_summaries <- epicsadata::get_summaries_data(country, station_id, summary = "annual_rainfall_summaries")
  summary_data <- get_summaries[[1]]
  timestamp <- get_summaries[[2]]

  # what if the definitions is different? Have an override option.
  # if the summary data exists, and if you do not want to override it then:
  if (nrow(summary_data) > 0 & override == FALSE) {
    # to see definitions that exist in the bucket / whether that definition exists under that ID
    file_name <- epicsadata::get_objects_in_bucket(country, definitions_id, timestamp = timestamp)
    if (nrow(file_name) == 0) {
      list_return[[1]] <- (definitions(country, definitions_id, summaries = summaries))
    } else {
      list_return[[1]] <- (definitions(country, definitions_id, summaries = summaries, paste0(definitions_id, ".", timestamp)))
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
    file_name <- epicsadata::get_objects_in_bucket(country, definitions_id, timestamp = timestamp)
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
    data_names <- epicsadata::data_definitions(names(daily), TRUE)
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
