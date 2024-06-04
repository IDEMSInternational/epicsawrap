#' Add New Definitions to Google Cloud Storage Bucket
#'
#' This function adds a new definitions file with a timestamp to a specified Google Cloud Storage (GCS) bucket. The new definitions are sourced from a JSON file provided as input.
#'
#' @param country A character vector specifying the country that the data corresponds to. Options are `"mz"` and `"zm"`.
#' @param definitions_id A character string specifying the ID of the station that the data corresponds to.
#' @param new_definitions A character vector specifying the path to the JSON file containing the new definitions.
#' @param timestamp A character vector with a timestamp. By default this is `NULL` so is generated at the time of creation.
#'
#' @export
#' @details
#' The function creates a timestamp in the format "YYYYMMDDHHMMSS" and appends it to the definitions_id to form the filename. It then reads the provided JSON file, creates a new JSON file with the timestamped filename, and uploads it to the specified GCS bucket.
add_definitions_to_bucket <- function (country, definitions_id, new_definitions, timestamp = NULL) {
  epicsadata:::add_definitions_to_bucket(country = country, station_id = definitions_id, new_definitions = new_definitions, timestamp = timestamp)
}
