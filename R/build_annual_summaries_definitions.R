#' Build Annual Summaries Definitions for Climatic Analysis
#'
#' Constructs a structured list of annual summary definitions for use in climate-related analyses.
#' These summaries typically include: start of rains, end of rains, end of season, seasonal length,
#' total rainfall, count of rain days, and number of extreme rain days. It supports both standard
#' and derived indicators, with optional metadata drawn from calculations in the raw dataset.
#'
#' @param data_name Name of the dataset being referenced. (Retained for compatibility; not currently used directly.)
#' @param data_by_year A named list of summary definitions from the yearly-aggregated dataset, typically from `get_r_instat_definitions()`.
#' @param rain_name Name of the rainfall column in the dataset.
#' @param definitions_in_raw (Optional) Raw definition metadata, typically obtained using `get_r_instat_definitions()` on the unaggregated dataset. Required if `extreme_rainfall_column` is used.
#' @param rain_days_name Name of the indicator used to define rain days (e.g., `"count"`). Default is `"count"`.
#' @param extreme_rain_name Name of the indicator used to define extreme rain days. Default is `"extreme_rain"`.
#' @param annual_total_rain_col Column name for total annual rainfall (optional).
#' @param seasonal_total_rain_col Column name for total seasonal rainfall (optional).
#' @param annual_rainday_col Column name for count of annual rain days (optional).
#' @param seasonal_rainday_col Column name for count of seasonal rain days (optional).
#' @param start_rains_column Column name representing the start of rains (DOY).
#' @param start_rains_status_column Column indicating status for start of rains.
#' @param end_rains_column Column name representing the end of rains (DOY).
#' @param end_rains_status_column Column indicating status for end of rains.
#' @param end_season_column Column name representing the end of the season (DOY).
#' @param end_season_status_column Column indicating status for end of season.
#' @param seasonal_length_column Column name representing seasonal length (in days).
#' @param extreme_rainfall_column (Optional) Name of the column used in the raw definitions to define extreme rainfall threshold (e.g., `"(rainfall >= 40)"`).
#' @param extreme_tmin_column (Optional) Name of the column used in the raw definitions to define extreme tmin threshold (e.g., `"(tmin <= 15)"`).
#' @param extreme_tmax_column (Optional) Name of the column used in the raw definitions to define extreme tmax threshold (e.g., `"(tmax >= 30)"`).
#'
#' @return A named list of annual summaries including:
#' \describe{
#'   \item{start_rains}{Definition for start of rains, with optional status logic.}
#'   \item{end_rains}{Definition for end of rains.}
#'   \item{end_season}{Definition for end of season.}
#'   \item{season_length}{Definition for seasonal length.}
#'   \item{annual_rain}{Definition for total rainfall and rain days.}
#'   \item{extreme_rain}{Definition for extreme rain day count, if available.}
#' }
#'
#' @details
#' This function merges multiple sub-definitions, combining both time-based and rainfall-based
#' summaries into one unified structure. If rain day or extreme rainfall definitions are specified,
#' it pulls metadata from `definitions_in_raw` to determine appropriate thresholds or logic.
#'
#' @examples
#' \dontrun{
#' defs <- build_annual_summaries_definitions(
#'   data_name = "ghana",
#'   data_by_year = get_r_instat_definitions("ghana_by_station_year"),
#'   rain_name = "rain",
#'   start_rains_column = "start_rains_doy",
#'   start_rains_status_column = "start_rain_status",
#'   end_rains_column = "end_rains_doy",
#'   end_rains_status_column = "end_rain_status",
#'   end_season_column = "end_season_doy",
#'   end_season_status_column = "end_season_status",
#'   seasonal_length_column = "season_length",
#'   extreme_rainfall_column = "extreme_rain",
#'   extreme_tmin_column = "extreme_tmin",
#'   extreme_tmax_column = "extreme_tmax"
#' )
#' }
#'
#' @export
build_annual_summaries_definitions <- function(data_name, data_by_year,
                                               rain_name = "rain", # the name of the rain name from our main data frame
                                               definitions_in_raw = NULL,
                                               rain_days_name = "count", # the name of the indicator for rainy days (e.g., count) created for the n_rain_days, etc from our main data frame
                                               extreme_rain_name = "extreme_rain", # the name of indicator for if it was an extreme rainy day or not, created for the n_rain_days, etc from our main data frame
                                               annual_total_rain_col = NULL, seasonal_total_rain_col = NULL,
                                               annual_rainday_col = NULL, seasonal_rainday_col = NULL,
                                               start_rains_column = NULL, start_rains_status_column = NULL,
                                               end_rains_column = NULL, end_rains_status_column = NULL, end_season_column = NULL,
                                               end_season_status_column = NULL, seasonal_length_column = NULL,
                                               extreme_rainfall_column = NULL, extreme_tmin_column = NULL, extreme_tmax_column = NULL){
  start_of_rains <- NULL
  end_rains <- NULL
  end_season <- NULL
  seasonal_length <- NULL
  total_rain_counts <- NULL
  extreme_rain_counts <- NULL
  extreme_tmin_counts <- NULL
  extreme_tmax_counts <- NULL
  if (!is.null(start_rains_column)) start_of_rains <- get_start_rains_definitions(data_by_year[[start_rains_column]])
  if (!is.null(end_rains_column)) end_rains <- get_end_rains_definitions(data_by_year[[end_rains_column]])
  if (!is.null(end_season_column)) end_season <- get_end_season_definitions(data_by_year[[end_season_column]])
  if (!is.null(seasonal_length_column)) seasonal_length <- get_season_length_definitions(data_by_year[[seasonal_length_column]])

  if (!is.null(start_rains_status_column) && !is.null(data_by_year[[start_rains_status_column]])) start_of_rains$start_rains$include_status <- TRUE
  if (!is.null(end_rains_status_column) && !is.null(data_by_year[[end_rains_status_column]])) end_rains$end_rains$include_status <- TRUE
  if (!is.null(end_season_status_column) && !is.null(data_by_year[[end_season_status_column]])) end_season$end_season$include_status <- TRUE
  
  # for annual rainfall / rainy days in year:
  total_rain_counts <- get_total_rain_counts(data_by_year, rain_name,
                                             annual_total_rain_col = annual_total_rain_col,
                                             seasonal_total_rain_col = seasonal_total_rain_col,
                                             annual_rainday_col = annual_rainday_col,
                                             seasonal_rainday_col = seasonal_rainday_col,
                                             definitions_in_raw = definitions_in_raw,
                                             rain_days_name = rain_days_name)

  # We want this to work for extreme_rain_name, but we need access to the raw data for this.
  # We don't need the raw data. Just access to it so we can get the calculations. Perhaps then this is optional to them if they want to 
  # give it or not. 
  # we can get the raw data frame if they supply the rain_days_name and extreme_rain_name (rain days), but, I don't know if this is OK
  # We only need the raw data for the summaries in it!
  # if they are protective over their raw data. (we do not need the data itself, just the summaries within it)
  extreme_rain_counts <- get_extreme_rain_counts(definitions_in_raw, extreme_rainfall_column)
  extreme_tmin_counts <- get_extreme_rain_counts(definitions_in_raw, extreme_tmin_column)
  extreme_tmax_counts <- get_extreme_rain_counts(definitions_in_raw, extreme_tmax_column)
  
  # Get the list of summaries:
  summaries_list <- c(start_of_rains, end_rains, end_season, seasonal_length, total_rain_counts, extreme_rain_counts, extreme_tmin_counts, extreme_tmax_counts)
  return(summaries_list)
}