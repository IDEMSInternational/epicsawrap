#' Get season length definitions
#'
#' Retrieves season length definitions.
#'
#' @param length The season length data.
#' @return A list representation of season length definitions.
#' @examples
#' # Example usage:
#' #get_season_length_definitions(length)
get_season_length_definitions <- function(length = NULL){ # TODO: it should be called "season" not "end_season"
  # Create an empty list
  data_list <- list()
  variables_list <- c("end_type")
  
  if (!is.null(length)) {
    end_type <- sub(" - .*", "", length[[3]])
    end_type <- sub(".*?_", "", end_type)
  }
  # Loop through variables and add to the list if defined
  for (variable in variables_list) {
    if (exists(variable) && !is.na(get(variable))) {
      data_list[["seasonal_length"]][[variable]] <- get(variable)
    } else {
      data_list[["seasonal_length"]][[variable]] <- NA
    }
  }
  return(data_list)
}