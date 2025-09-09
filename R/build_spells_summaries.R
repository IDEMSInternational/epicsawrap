#' Build Spells Definitions from File
#'
#' This function reads spell data from a provided file structure generated in R-Instat.
#' It then extracts information about the parameter values given to construct these spells.
#'
#' @param spells_data Data frame used for spells data (usually `spells`).
#' 
#' @return A list representing the structured crop definition data, including water requirements, 
#' planting dates, and planting length.
#'
#' @examples
#' # Assuming definition_file is a correctly structured list:
#' #get_crop_definitions(definition_file)
build_spells_summaries <- function(spells_data = NULL){
  spells_definitions <- get_r_instat_definitions(data_book$get_calculations(spells_data))
  
  # Create an empty list
  data_list <- list()
  data_list[["spells"]] <- list()
  # Create a list
  variables_list = c("start_day", "end_day", "sign", "from", "to")
  
  # TODO: not working for start/end day in R-Instat at the moment.
  spells_calculation <- spells_definitions$filter$spell[[2]]
  
    from <- extract_value(spells_calculation, " >= ")
    to <- extract_value(spells_calculation, " <= ")
    
    if (!is.na(from) && !is.na(to)){
      sign_check <- grepl("&", spells_calculation)
      if (sign_check) sign <- "between"
      else (sign <- "unknown")
    } else if (is.na(from) && !is.na(to)){
      sign <- "equal to or less than"
    } else if (!is.na(from) && is.na(to)){
      sign <- "equal to or greater than"
    } else if (is.na(from) & is.na(to)){
      from <- extract_value(spells_calculation, " > ")
      to <- extract_value(spells_calculation, " < ")
      sign <- grepl("|", spells_calculation)
      if (sign) sign <- "excluding between"
      else (sign <- "unknown")
    } else {
      from <- NA
      to <- NA
      sign <- "unknown"
    }
    
  # Loop through each variable in the list
  for (variable in variables_list) {
    # Check if the variable exists and is not NA
    if (exists(variable) && !is.na(get(variable))) {
      # Retrieve the variable's value
      variable_value <- get(variable)
      
      # Check if the variable's class includes "instat_calculation"
      if ("instat_calculation" %in% class(variable_value)) {
        data_list[["spells"]][[variable]] <- NA
      } else {
        data_list[["spells"]][[variable]] <- variable_value
      }
    } else {
      # Assign NA if the variable does not exist or is NA
      data_list[["spells"]][[variable]] <- NA
    }
  }
  return(data_list)
}
