#' Get the Extreme Data for Rainfall
#'
#' This function identifies extreme values for rainfall in a data frame.
#' 
#' @param country A character string specifying the country code of the data.
#' @param station_id A character vector specifying the ID(s) of the station(s) to analyse.
#'
#' @return A data frame containing the extreme data.
#' @export
#'
#' @examples
#' # Generate annual temperature summaries for station 16 in Zambia
#' #extremes_rain(country, station_id)
extremes_rain <- function(country, station_id){
  return(epicsawrap:::overall_extremes_summaries(country = country, station_id = station_id, summaries = "extremes_rain"))
}
