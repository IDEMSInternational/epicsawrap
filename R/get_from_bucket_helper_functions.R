# --- helpers ---------------------------------------------------------------
#' Get Bucket Name
#'
#' @param country A character vector specifying the country or countries from which to update the data. Options are `"mz"`, `"zm"`, `"zm_test"`, `"ml_test"`, `"ke_test"`.
#'
#' @return Returns the name of the Google Bucket for the data.
get_bucket_name <- function(country = c("mw", "zm", "zm_test", "ml_test", "mw_test", "ke_test", "internal_tests", "zm_workshops", "mw_workshops")) {
  if (length(country) > 1) stop("'country' must be length 1.")
  country <- match.arg(country)
  if (country == "mw") return("malawi_data")
  else if (country == "zm") return("zambia_data")
  else if (country == "zm_test") return("zambia_test_data")
  else if (country == "ml_test") return("malawi_test_data")
  else if (country == "mw_test") return("malawi_test_data")
  else if (country == "ke_test") return("kenya_test_data")
  else if (country == "internal_tests") return("climsoft_data")
  else if (country == "zm_workshops") return("zm_workshops")
  else if (country == "mw_workshops") return("mw_workshops")
}

# R code that returns the name of different definitions.
pick_latest_id <- function(json_paths) {
  if (length(json_paths) == 0) return(NULL)
  # expects something like "definitions/ABC.2025...Z.json"
  extract_most_recent_json(json_paths)
}

resolve_latest_for <- function(country, definition_id) {
  jsons <- list_definition_versions(country, definition_id)
  if (length(jsons) == 0) {
    stop("No JSON files found in bucket for '", definition_id, "'. Check country/IDs.", call. = FALSE)
  }
  pick_latest_id(jsons)
}

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