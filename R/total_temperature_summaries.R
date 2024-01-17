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
#' #total_temperature_summaries(country, station_id, c("mean_tmin", "mean_tmax"), to = "annual")
total_temperature_summaries <- function(country, station_id,
                                        summaries = c("mean_tmin", "mean_tmax", "min_tmin", "min_tmax", "max_tmin", "max_tmax"),
                                        to = c("annual", "monthly")) {
  to <- match.arg(to)
  daily <- epicsadata::get_daily_data(country = country, station_id = station_id)
  definitions <- epicsawrap::definitions(country = country, station_id = station_id, summaries = summaries)
  data_names <- epicsadata::data_definitions(station_id = station_id)
  
  summary_data_list <- list()
  
  for (summary in summaries) {
    if (is.null(definitions[[summary]]$to)) {
      stop(paste0("'", summary, "' has no data in definitions json file."))
    }
    if (!grepl(to, x = definitions[[summary]]$to)) next
    
    na_rm_flag <- if (is.null(definitions[[summary]]$na_rm)) {
      warning("Missing na_rm in ", summary, ", setting as FALSE.")
      FALSE
    } else {
      is.logical(definitions[[summary]]$na_rm)
    }
    
    temp_col <- if (grepl("_tmin", summary)) data_names$tmin else if (grepl("_tmax", summary)) data_names$tmax else NULL
    summary_func <- if (grepl("mean_", summary)) "mean" else sub("_tmax", "", sub("_tmin", "", summary))
    
    summary_data_list[[summary]] <- rpicsa:::summary_temperature(data = daily,
                                                                 date_time = data_names$date,
                                                                 station = data_names$station,
                                                                 year = data_names$year,
                                                                 tmax = if (temp_col == "tmax") data_names$tmax else NULL,
                                                                 tmin = if (temp_col == "tmin") data_names$tmin else NULL,
                                                                 summaries = summary_func,
                                                                 to = to,
                                                                 na_rm = na_rm_flag,
                                                                 na_prop = definitions[[summary]]$na_prop,
                                                                 na_n = definitions[[summary]]$na_n,
                                                                 na_consec = definitions[[summary]]$na_consec,
                                                                 na_n_non = definitions[[summary]]$na_n_non)
  }
  
  
  merge_dfs <- function(df1, df2) {
    dplyr::full_join(df1, df2, by = c(data_names$year, data_names$station))
  }
  
  # Using Reduce to iteratively merge all data frames in the list
  summary_data <- Reduce(merge_dfs, summary_data_list)
  
  summary_data$year <- as.integer(summary_data$year)
  if ("month" %in% names(summary_data)) {
    summary_data$month <- as.integer(forcats::as_factor(summary_data$month))
  }
  
  return(list(definitions, summary_data))
}