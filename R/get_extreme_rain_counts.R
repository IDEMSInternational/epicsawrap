#' Extract Extreme Rainfall Threshold Definition
#'
#' This is a convenience wrapper around [get_rain_counts()] specifically for extracting
#' the threshold and comparison operator used to define an "extreme rain day" condition,
#' such as `"(rainfall >= 40)"`. The result is returned as a list nested under the
#' `"extreme_rain"` key.
#'
#' @param definitions_in_raw A named list of raw definitions, typically obtained from
#'   `get_r_instat_definitions()` on the unaggregated dataset.
#' @param extreme_rainfall_column The name of the rainfall definition to extract from.
#'   Default is `"extreme_rain"`.
#' @param list_name The name to give the new item in the list.
#'
#' @return A list with one element named `"extreme_rain"`, containing:
#' \describe{
#'   \item{sign}{The extracted comparison operator (e.g., `>=`, `<`).}
#'   \item{threshold}{The threshold value as a character string (e.g., `"40"`).}
#' }
#' If the definition is missing or malformed, both elements are returned as `NA`.
#'
#' @examples
#' defs <- list(extreme_rain = list(rain_day = list(NULL, "(rainfall >= 40)")))
#'
#' # Returns: list(extreme_rain = list(sign = ">=", threshold = "40"))
#' get_extreme_rain_counts(defs)
#'
#' @export
get_extreme_rain_counts <- function(definitions_in_raw = NULL,
                                    extreme_rainfall_column = "extreme_rain",
                                    list_name = "extreme_rain") {
  get_rain_counts(definitions_in_raw, extreme_rainfall_column, list_name = list_name)
}