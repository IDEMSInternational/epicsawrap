#' Get Daily Data
#'
#' @param country A character vector specifying the country or countries from which to get the data. Common options are `"mz"`, `"zm"`, and `"zm_test"`. Any defined in `get_bucket_name()`.
#' @param station_id A character string specifying the ID of the station for which to get the daily data.
#'
#' @return A data frame containing the daily data for the specified station and country.
#' @export
#'
#' @examples #
get_daily_data <- function(country, station_id, call_from = c("climsoft", "googlebuckets")) {
  call_from <- match.arg(call_from)
  if (length(country) > 1) stop("'country' must be of length 1")
  station_id <- as.character(station_id)
  
  if (call_from == "climsoft"){
    # if you call from climsoft
    climsoft_info <- station_metadata(country = country, station_id = station_id)$climsoft_list
    if (is.null(get_climsoft_conn())) stop("Set climsoft connection with set_climsoft_conn() function.")
    station_data <- import_from_climsoft(con = get_climsoft_conn(),
                                         stations = station_id,
                                         include_station_info = FALSE,
                                         elementfiltercolumn = climsoft_info[[1]]$elementfiltercolumn,
                                         elements = climsoft_info[[1]]$elements)
  } else {
    # if you call from googlebuckets
    dfs <- vector("list", length(station_id))
    names(dfs) <- station_id
    for (i in seq_along(station_id)) {
      f <- paste0(country, "/", "data", "/", station_id[i], ".rds")
      if (file.exists(f)) {
        dfs[[i]] <- readRDS(f)
      } else {
        f <- epicsadata::update_daily_data(country, station_id[i])
        dfs[[i]] <- f#saveRDS(o, file = f)
      }
    }
    if (length(station_id) > 1) {
      station_data <- dplyr::bind_rows(dfs)
    } else station_data <- dfs[[1]] 
  }
  return(station_data)
}
