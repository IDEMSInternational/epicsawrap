#' Get the Extreme Data for Maximum Temperature
#'
#' This function identifies extreme values for naximum temperature in a data frame.
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
extremes_tmax <- function(country, station_id){
  return(overall_extremes_summaries(country = country, station_id = station_id, summaries = "extremes_tmax"))
}