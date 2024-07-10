#' Get the Summaries Data
#' 
#' @description This function updates the summary data for a specific station in the specified country. It retrieves the data from Google Cloud Storage using the `get_data` function.
#'
#' @param country A character vector specifying the country or countries from which to get the data. Options are defined in `get_bucket_name()` (e.g., `"zm"`, `"mw"`).
#' @param station_id A character string specifying the ID of the station for which to get the summary data.
#' @param summary A character string specifying the summary to retrieve.
#' 
#' @details 
#' The `update_daily_data` function is used to update the daily data for a specific station in the specified country. It internally calls the `get_data` function to retrieve the data from Google Cloud Storage.
#' The `country` argument is a character vector that allows specifying one or more countries from which to update the data. The data will be updated for Mozambique (`"mz"`) and Zambia (`"zm"`). You can modify this argument to update data for different countries.
#' The `station_id` argument is a character string that specifies the ID of the station for which to update the daily data. The function will construct the filename by concatenating the `"data/"` directory, the `station_id`, and the `file` extension `".rds"`. The filename will be passed to the `get_data` function to retrieve the data.
#' The function uses the invisible function to suppress the output of the `get_data` function, ensuring that the data retrieval process is not visible in the console.
#'
#' @return This function does not return any value explicitly. It gets the summary data for the specified station in the specified country.
#' @export
#'
#' @examples # todo
update_summaries_data <- function (country, station_id, summary){
  filename <- paste0("summaries", "/", summary, "_", station_id, ".rds")
  invisible(get_data(country = country, filename = filename))
}