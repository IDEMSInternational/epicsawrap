#' Generate Annual and Monthly Temperature Summaries
#'
#' This function calculates annual or monthly temperature summaries for specified stations in a given country.
#'
#' @param country A character string specifying the country code of the data.
#' @param station_id A character vector specifying the ID(s) of the station(s) to analyze.
#' @param summaries A character vector specifying the names of the summaries to produce.
#' @param to A character string indicating whether the summaries should be generated for "annual" or "monthly" data.
#'
#' @return A data frame containing the requested temperature summaries.
#'
#' @examples
#' # Generate annual temperature summaries for station 16 in Zambia
#' #total_temperature_summaries(country = "zm", station_id = "16", to = "annual")
total_temperature_summaries <- function(country,
                                        station_id,
                                        summaries = c("mean_tmin", "mean_tmax"),
                                        to = c("annual", "monthly")) {
  to <- match.arg(to)
  daily <- epicsadata::get_daily_data(country = country, station_id = station_id)
  definitions <- epicsawrap::definitions(country = country, station_id = station_id, summaries = summaries)
  data_names <- epicsadata::data_definitions(station_id = station_id)
  
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
      if (grepl(to, x = definitions[[summary]]$to)){
        if (is.null(definitions[[summary]]$na_rm)){
          warning("Missing parameter value for na_rm in ", summary, ". Setting as FALSE.")
          definitions$annual_rain$na_rm <- FALSE
        }
        summary_data[[summary]] <- rpicsa::mean_temperature(data = daily,
                                                 date_time = data_names$date,
                                                 station = data_names$station,
                                                 year = data_names$year,
                                                 tmax = if (summary == "mean_tmax") data_names$tmax else NULL,
                                                 tmin = if (summary == "mean_tmin") data_names$tmin else NULL,
                                                 to = to,
                                                 na_rm = is.logical(definitions[[summary]]$na_rm),
                                                 na_prop = definitions[[summary]]$na_prop,
                                                 na_n = definitions[[summary]]$na_n,
                                                 na_consec = definitions[[summary]]$na_consec,
                                                 na_n_non = definitions[[summary]]$na_n_non)
      }
    }
  }
  if (length(summary_data) == 2){ 
    summary_data <- dplyr::full_join(summary_data[[1]], summary_data[[2]])
  } else {
    summary_data <- summary_data[[1]]
  }
  summary_data$year <- as.integer(summary_data$year)
  if ("month" %in% names(summary_data)){
    summary_data$month <- as.integer(forcats::as_factor(summary_data$month))
  }
  list_return <- list(definitions, summary_data)
  return(list_return) # return a list with the metadata and the data itself
}
