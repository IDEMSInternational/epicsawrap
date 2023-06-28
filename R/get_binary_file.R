#' Update PDF/JPEG Data
#' 
#' @description This function updates the PDF/JPEG data for a specific station in the specified country. It retrieves the data from Google Cloud Storage using the `get_data` function.
#'
#' @param country A character vector specifying the country or countries from which to update the data. Options are `"mz"` and `"zm"`.
#' @param station_id A character string specifying the ID of the station for which to update the daily data.
#' @param type A character string specifying whether the data to retrieve is JPEG or PDF.
#' 
#' @details 
#' The `country` argument is a character vector that allows specifying one or more countries from which to get the PDF/JPEG data. The data will be updated for Mozambique (`"mz"`) and Zambia (`"zm"`). You can modify this argument to update data for different countries.
#' The `station_id` argument is a character string that specifies the ID of the station for which to update the PDF/JPEG data. The function will construct the filename by concatenating the `"pdf/"` or `"jpeg"/` directory, the `station_id`, and the `file` extension. The filename will be passed to the `get_data` function to retrieve the data.
#' The function uses the invisible function to suppress the output of the `get_data` function, ensuring that the data retrieval process is not visible in the console.
#'
#' @return This function does not return any value explicitly. It gets the PDF/JPEG data for the specified station in the specified country.
#' @export
#'
#' @examples # get_binary_file("zm", "16", "pdf")
get_binary_file <- function(country = c("mz", "zm"), station_id, type = c("pdf", "jpeg")) {
  return(epicsadata::get_binary_file(country = country, station_id = station_id, type = type))
}
