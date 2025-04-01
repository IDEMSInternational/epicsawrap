#' Build Crop Definitions from File
#'
#' This function reads crop definition data from a provided file structure generated in R-Instat.
#' It then extracts information about water requirements, planting dates, and planting length for different crops.
#' The extracted values are then split into lists..
#'
#' @param definition_file A list containing file data and attributes generated in R-Instat with
#' named vectors `Var1`, `Var2`, and `Var3` for water requirements, planting dates, and planting length respectively.
#'
#' @return A list representing the structured crop definition data, including water requirements, 
#' planting dates, and planting length.
#'
#' @examples
#' # Assuming definition_file is a correctly structured list:
#' #get_crop_definitions(definition_file)
build_crop_definitions <- function(definition_file = NULL){
  variables_list <- c("water_requirements", "planting_dates", "planting_length")
  data_list <- list()
  
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
    water_requirements <- get_seq_values(unique(definition_file$rain_total))
    planting_dates <- get_seq_values(unique(definition_file$plant_day))
    planting_length <- get_seq_values(unique(definition_file$plant_length))
  }
  
  # Loop through variables and add to the list if defined
  for (variable in variables_list) {
    if (exists(variable) && !anyNA(get(variable))) {
      data_list[["crops_success"]][[variable]] <- get(variable)
    } else {
        data_list[["crops_success"]][[variable]] <- NA
    }
  }
  return(data_list)
}
