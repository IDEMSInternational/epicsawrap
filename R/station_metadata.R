#' Get Station Metadata
#'
#' @param country `character(1)` The country code of the data.
#'
#' @return
#' @export
#'
#' @examples
station_metadata <- function(country) {
  epicsadata::station_metadata(country)
}