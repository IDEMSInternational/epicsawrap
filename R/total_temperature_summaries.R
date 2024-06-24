#' Generate Annual and Monthly Temperature Summaries
#'
#' This function calculates annual or monthly temperature summaries for specified stations in a given country.
#'
#' @param country A character string specifying the country code of the data.
#' @param station_id A character vector specifying the ID(s) of the station(s) to analyse.
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
                                        summaries = c("mean_tmin", "mean_tmax", "min_tmin", "min_tmax", "max_tmin", "max_tmax"),
                                        to = c("annual", "monthly"),
                                        override = FALSE) {
  to <- match.arg(to)
  list_return <- NULL
  
  # we get the definitions_id from station_id metadata.
  definitions_id <- get_definitions_id_from_metadata(country, station_id)
  
  # do the summaries exist already?
  get_summaries <- epicsadata::get_summaries_data(country, station_id, summary = paste0(to, "_temperature_summaries"))
  summary_data <- get_summaries[[1]]
  # what if the definitions is different? Have an override option.
  # if the summary data exists, and if you do not want to override it then:
  if (nrow(summary_data) > 0 & override == FALSE) {
    file_name <- epicsadata::get_objects_in_bucket(country, definitions_id, timestamp = get_summaries[[2]])
    if (nrow(file_name) == 0) {
      list_return[[1]] <- (definitions(country, definitions_id, summaries = summaries))
    } else {
      list_return[[1]] <- (definitions(country, definitions_id, summaries = summaries, paste0(definitions_id, ".", get_summaries[[2]])))
    }
    
    # set vars_to_pull to only be names in the summary_data
    vars_to_pull <- c("station", "year", "month", summaries)
    vars_to_pull <- vars_to_pull[vars_to_pull %in% colnames(summary_data)]
    summary_data <- summary_data %>% dplyr::select(dplyr::all_of(vars_to_pull))
    
  } else {
    definitions <- definitions(country = country, definitions_id, summaries = summaries)
    # Fetch daily data and preprocess
    daily <- epicsadata::get_daily_data(country = country, station_id = station_id)
    
    # For the variable names to be set as a certain default, set TRUE here, and run check_and_rename_variables
    data_names <- epicsadata::data_definitions(names(daily), TRUE)
    daily <- check_and_rename_variables(daily, data_names)
    
    # # even though we can have tmax and tmin defined together, it's being done this way 
    # # in case different parameters are defined for tmax and for tmin.
    # summary_data <- expand.grid(year = unique(daily[[data_names$year]]), 
    #                             station = unique(daily[[data_names$station]]))
    # names(summary_data) <- c(data_names$year, data_names$station)
    # 
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
