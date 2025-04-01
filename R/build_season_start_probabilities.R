#' Build Season Start Probabilities from File
#'
#' This function processes a file structure to extract information about the specified day for season start probabilities.
#' The information is split into lists
#'
#' @param definition_file A list containing file data and attributes generated in R-Instat.
#'
#' @return A list representing the season start probabilities with the specified days.
#'
#' @examples
#' #get_season_start_probabilities(definition_file)
build_season_start_probabilities <- function(definition_file = NULL){
  
  # Create an empty list
  data_list <- list()
  data_list[["season_start_probabilities"]] <- list()
  
  get_seq_values <- function(value) {
    if (length(value) < 3) return(value)
    
    from <- value[1]
    to <- value[length(value)]
    by <- (to - from) / (length(value) - 1)
    
    # Reconstruct the sequence
    reconstructed <- seq(from, to, by)
    
    # Check if reconstructed sequence exactly matches original
    if (all.equal(value, reconstructed) == TRUE) {
      return(list(from = from, to = to, by = by))
    } else {
      return(value)
    }
  }
  
  if (!is.null(definition_file)){
    specified_day <- get_seq_values(unique(definition_file$plant_day))
    data_list[["season_start_probabilities"]][["specified_day"]] <- specified_day
  } else {
    data_list[["season_start_probabilities"]][["specified_day"]] <- NA
  }
  return(data_list)
}

