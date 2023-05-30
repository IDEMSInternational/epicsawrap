#' Annual and Monthly Temperature Summaries
#'
#' @param country `character(1)` The country code of the data.
#' @param station_id `character` The id's of the stations to analyse. Either a
#'   single value or a vector.
#' @param summaries `character` The names of the summaries to produce.
#' @param to `character` Whether this is for annual or monthly summaries
#'
#' @return A data frame with either annual or monthly summaries.
#' @export
#'
#' @examples
#' # total_temperature_summaries(country = "zm", station_id = "16") # made a fake "16" json definitions data
#' # because it contains temperature data. 
total_temperature_summaries <- function(country,
                                        station_id,
                                        summaries = c("mean_tmin",
                                                      "mean_tmax"),
                                        to = c("annual", "monthly")) {
  to <- match.arg(to)
  daily <- epicsadata::get_daily_data(country = country, station_id = station_id)
  definitions <- epicsawrap::definitions(country = country, station_id = station_id, summaries = summaries)
  
  # even though we can have tmax and tmin defined together, it's being done this way 
  # in case different paramters are defined for tmax and for tmin.
  # TODO: check if that is sensible. 
  summary_data <- expand.grid(year = unique(daily$year), station = unique(daily$station))
  if ("mean_tmax" %in% summaries){
    if (is.null(definitions$mean_tmax$to)){
      stop("'mean_tmax' has been given in summaries but no data is given in definitions json file.")
    } else {
      if (to == "annual"){
        if (grepl('annual', x = definitions$mean_tmax$to)){
          # TODO: what if different variable names?
          summary_tmax <- rpicsa::mean_temperature(data = daily,
                                                   date_time  = "date",
                                                   station = "station_name",
                                                   year = "year",
                                                   tmax = "tmax",
                                                   tmin = NULL,
                                                   to = "annual",
                                                   na_rm = is.logical(definitions$mean_tmax$na_rm),
                                                   na_prop = definitions$mean_tmax$na_prop,
                                                   na_n = definitions$mean_tmax$na_n,
                                                   na_consec = definitions$mean_tmax$na_consec,
                                                   na_n_non = definitions$mean_tmax$na_n_non)
          summary_data <- dplyr::full_join(summary_data, summary_tmax)
        }
      } else {
        if (grepl('monthly', x = definitions$mean_tmax$to)){
          summary_tmax <- rpicsa::mean_temperature(data = daily,
                                                   date_time  = "date",
                                                   station = "station_name",
                                                   year = "year",
                                                   tmax = "tmax",
                                                   tmin = NULL,
                                                   to = "monthly",
                                                   na_rm = is.logical(definitions$mean_tmax$na_rm),
                                                   na_prop = definitions$mean_tmax$na_prop,
                                                   na_n = definitions$mean_tmax$na_n,
                                                   na_consec = definitions$mean_tmax$na_consec,
                                                   na_n_non = definitions$mean_tmax$na_n_non)
          summary_data <- dplyr::full_join(summary_data, summary_tmax)
        } 
      }
    }
  }
  if ("mean_tmin" %in% summaries){
    if (is.null(definitions$mean_tmin$to)){
      stop("'mean_tmin' has been given in summaries but no data is given in definitions json file.")
    } else {
      if (to == "annual"){
        if (grepl('annual', x = definitions$mean_tmin$to)){
          summary_tmin <- rpicsa::mean_temperature(data = daily,
                                                   date_time  = "date",
                                                   station = "station_name",
                                                   year = "year",
                                                   tmax = NULL,
                                                   tmin = "tmin",
                                                   to = "annual",
                                                   na_rm = is.logical(definitions$mean_tmin$na_rm),
                                                   na_prop = definitions$mean_tmin$na_prop,
                                                   na_n = definitions$mean_tmin$na_n,
                                                   na_consec = definitions$mean_tmin$na_consec,
                                                   na_n_non = definitions$mean_tmin$na_n_non)
          summary_data <- dplyr::full_join(summary_data, summary_tmin)
        }
      } else {
        if (grepl('monthly', x = definitions$mean_tmin$to)){
          summary_tmin <- rpicsa::mean_temperature(data = daily,
                                                   date_time  = "date",
                                                   station = "station_name",
                                                   year = "year",
                                                   tmax = NULL,
                                                   tmin = "tmin",
                                                   to = "monthly",
                                                   na_rm = is.logical(definitions$mean_tmin$na_rm),
                                                   na_prop = definitions$mean_tmin$na_prop,
                                                   na_n = definitions$mean_tmin$na_n,
                                                   na_consec = definitions$mean_tmin$na_consec,
                                                   na_n_non = definitions$mean_tmin$na_n_non)
          summary_data <- dplyr::full_join(summary_data, summary_tmin)
        } 
      }
    }
  }
  list_return <- NULL
  list_return[[1]] <- c(definitions)
  list_return[[2]] <- summary_data
  return(list_return) # return a list with in it the metadata and the data itself
}
