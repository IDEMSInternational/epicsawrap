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
