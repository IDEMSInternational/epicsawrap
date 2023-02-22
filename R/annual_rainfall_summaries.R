#' Annual Rainfall Summaries
#'
#' @param station_id `character` The id's of the stations to analyse. Either a
#'   single value or a vector.
#' @param summaries `character` The names of the summaries to produce.
#'
#' @return A data frame with yearly summaries.
#' @export
#'
#' @examples
#' annual_rainfall_summaries(station_id = "1357")
annual_rainfall_summaries <- function(station_id, 
                                      summaries = c("seasonal_rainfall",
                                                    "seasonal_raindays",
                                                    "start_rains",
                                                    "end_season",
                                                    "length_season")) {
  df <- data.frame(station_id = rep(station_id, each = 30),
                   year = rep(1991:2020, 2),
                   seasonal_rainfall = stats::rnorm(60, 400, 70))
  return(df)
}