#' Get Binary File
#'
#' Reads and returns the binary data of a file.
#'
#' @param file_path The path to the file.
#' @return A raw vector containing the binary data of the file.
#' @export
#'
#' @examples # to complete
#' #pdf_data <- get_binary_file("data/idems_logo.jpg")
#' #pdf_data <- get_binary_file("data/idems_logo.pdf")
#' 
get_binary_file <- function(file_path) {
  return(rpicsa::get_binary_file(file_path = file_path))
}