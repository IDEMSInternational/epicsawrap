#' Extract the most recent JSON file from a list of filenames
#' 
#' This function takes a list of filenames as input and returns the filename 
#' of the most recent JSON file. The filenames should be in the format 
#' "definitions/1.YYYYMMDDHHMMSS.json".
#' 
#' @param files A character vector containing filenames.
#' 
#' @return A character string representing the filename of the most recent JSON file.
#' 
#' @examples
#' files <- c(
#'   "definitions/1.20240116155433.json",
#'   "definitions/1.20240304125111.json",
#'   "definitions/1.json"
#' )
#' epicsadata:::extract_most_recent_json(files)
extract_most_recent_json <- function(files) {
  most_recent_file <- NULL
  most_recent_timestamp <- 0
  
  for (file in files) {
    timestamp_str <- unlist(strsplit(file, "[.]"))[2]
    timestamp <- as.numeric(timestamp_str)
    
    if (!is.na(timestamp) && timestamp > most_recent_timestamp) {
      most_recent_timestamp <- timestamp
      most_recent_file <- file
    }
  }
  most_recent_file <- gsub("definitions/", "", most_recent_file)
  most_recent_file <- gsub(".json$", "", most_recent_file)
  return(most_recent_file)
}
