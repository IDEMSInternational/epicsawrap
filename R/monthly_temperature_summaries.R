#' Monthly Temperature Summaries
#'
#' @param country `character(1)` The country code of the data.
#' @param station_id `character` The id's of the stations to analyse. Either a
#'   single value or a vector.
#' @param summaries `character` The names of the summaries to produce.
#' @param override A logical argument default `FALSE` indicating whether to calculate the summaries still, even if they are stored already in the bucket.
#' 
#' @return A data frame with monthly summaries.
#' @export
#'
#' @examples
#' # monthly_temperature_summaries(country = "zm", station_id = "1", summaries = c("min_tmin"))
#' # because it contains temperature data. 
monthly_temperature_summaries <- function(country,
                                         station_id,
                                         summaries = c("mean_tmin","mean_tmax", "min_tmin", "min_tmax", "max_tmin", "max_tmax"), override = FALSE) {
  return(total_temperature_summaries(country = country, station_id = station_id, summaries = summaries, to = "monthly", override = override))
}
