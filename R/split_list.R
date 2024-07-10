#' Split Parameter Into List
#'
#' This function takes a vector where each element is a string formatted as "identifier=val"
#' and splits each element into an identifier and its corresponding numeric value. Each numeric value is
#' then stored in a list with a dynamically generated name based on its index.
#'
#' @param parameter A character vector where each element is a string with the format "identifier=value".
#'
#' @return A list where each element is the numeric value extracted from the input vector, 
#' named dynamically as "val1", "val2", etc., corresponding to their original order in the input vector.
#' 
#' @examples
#' #parameter <- c("A1=100", "A2=200", "A3=150")
#' #split_list(parameter)
split_list <- function(parameter){
  # Split each element of the vector on the equals sign
  split_values <- strsplit(parameter, "=")
  
  # Initialize an empty list to store your results
  result_list <- list()
  
  # Loop through the split values and assign them to the result list
  for(i in 1:length(split_values)) {
    # Trim the identifier and value parts to remove any leading/trailing spaces
    numeric_value <- as.numeric(trimws(split_values[[i]][2]))
    
    # Assign the numeric value to the result list using the identifier as the name
    result_list[[paste0("val", i)]] <- numeric_value
  }
  return(result_list)
}