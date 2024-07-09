#' Update Station Metadata
#'
#' @param country A character vector specifying the country or countries from which to update the metadata. Options are defined in `get_bucket_name()` (e.g., `"zm"`, `"mw"`).
#'
#' @return This function updates the metadata for the specified station in the specified country.
#' @export
#'
#' @examples #todo
update_metadata <- function(country) {
  filename <- paste0("metadata", ".rds")
  saveto <- paste0(country, "/", filename)
  invisible(get_data(country = country,  filename = filename))
}