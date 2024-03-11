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
annual_rainfall_summaries <- function(country, station_id, summaries = c("annual_rain", "start_rains", "end_rains", "end_season", "seasonal_rain", "seasonal_length"), override = FALSE) {
  list_return <- NULL
  
  # do the summaries exist already?
  summary_data <- epicsadata::get_summaries_data(country, station_id, summary = "annual_rainfall_summaries")
  
  # what if the definitions is different? Have an override option.
  # if the summary data exists, and if you do not want to override it then:
  if (nrow(summary_data) > 0 & override == FALSE) {
    list_return[[1]] <- "definition from summary"
  } else {
    # Get data definitions and summary definitions
    definitions <- definitions(country = country, station_id = station_id, summaries = summaries)
    definitions_season <- NULL
    
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
          definitions_2 <- definitions(country = country, station_id = station_id, summaries = paste0("end_", def_end_type))
          
          # if there's no definitions file, then set the end type to be the other one and check for definitions file
          if (length(definitions_2) == 0){ 
            warning(paste0("Ignoring end_type = ", def_end_type, " in seasonal_rain because no definitions given."))
            if (def_end_type == "rains"){
              definitions[[i]]$end_type <- "season"
              definitions_2 <- definitions(country = country, station_id = station_id, summaries = paste0("end_season"))
              if (length(definitions_2) == 0) stop(paste0("Cannot calculate", i, "without end_rains or end_season in definitions file."))
            } else {
              definitions[[i]]$end_type <- "rains"
              definitions_2 <- definitions(country = country, station_id = station_id, summaries = paste0("end_rains"))
              if (length(definitions_2) == 0) stop(paste0("Cannot calculate", i, "without end_rains or end_season in definitions file."))
            }
          } else {
            definitions <- c(definitions, definitions_2)
          }
        } 
        
        # if no end type is specified, then...
        if (is.null(def_end_type)){
          # is either end_* in the summaries?
          if (any(grepl("seasonal_", summaries)) || (any(grepl("end_", summaries)))){
            #great, we have at least one given so we're happy.
            # set end_type as season by default
            definitions[[i]]$end_type <- "season"
          } else { # if no end type is specified, and no summaries are specified
            # specified in the code asked for (if both, then end_seasons as per Roger's recommendation)
            definitions_season <- definitions(country = "zm", station_id = "1", summaries = c("end_rains", "end_season"))
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
    daily <- epicsadata::get_daily_data(country = country, station_id = station_id)
    # For the variable names to be set as a certain default, set TRUE here, and run check_and_rename_variables
    data_names <- epicsadata::data_definitions(names(daily), TRUE)
    daily <- check_and_rename_variables(daily, data_names)
    
    # Check if start_rains and end_rains are required for seasonal_rain and seasonal_length
    if (any(grepl("seasonal_", summaries))){
      if (!"start_rains" %in% summaries){
        summaries <- c(summaries, "start_rains")
        
        # checking we have a definitions file
        definitions_2 <- definitions(country = country, station_id = station_id, summaries = "start_rains")
        
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
