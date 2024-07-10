#' Extract Value
#'
#' Extracts a specific value from a string using a regular expression.
#'
#' @param string The input string.
#' @param value_expr The regular expression pattern to extract the value.
#' @param as_numeric Logical indicating whether the extracted value should be converted to numeric.
#' @return The extracted value.
#' @export
#' 
#' @examples
#' # Example usage:
#' extract_value("Example string with value: 123.45", "\\d+(\\.\\d+)?")
extract_value <- function(string, value_expr, as_numeric = TRUE){
  if (as_numeric){
    value <- stringr::str_match(string, paste0(value_expr, "([0-9]+(?:\\.[0-9]+)?)"))[1, 2]
    value <- as.numeric(value)
  } else {
    value <- gsub("\\)", "", stringr::str_match(string, paste0(value_expr, "([^\\s,]+)")))[1, 2]
  }
  return(value)
}