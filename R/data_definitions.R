#' Get Data Column Names
#'
#' This function takes a vector of variable names and standardises them by mapping
#' various variations of variable names to their corresponding standard names.
#'
#' @param data_names A character vector containing the variable names in the dataset.
#' @param rename_vars Logical value indicating whether to rename variables to their
#'        corresponding standard names. If `TRUE`, the function returns a list of renamed
#'        variables; if `FALSE`, it returns a list of mappings from variations to standard names.
#' @param exact_match Logical value indicating whether to perform an exact match for variable
#'        name variations or not. If `TRUE`, it matches only exact variable names; if `FALSE`, it
#'        performs a partial match.
#'
#' @return A list of standard variable names or a list of mappings from variations to
#'         standard names, depending on the value of `rename_vars`.
#'
#' @export
#'
#' @examples
#' # Example 1: Redefine variables without renaming
#' data_definitions(c("station_id", "station_", "date", "year", "month",
#'                        "day", "doy", "rain", "tmin", "tmax"), 
#'                        rename_vars = FALSE, exact_match = FALSE)
#' 
#' # Example 2: Redefine variables with renaming
#' data_definitions(c("date", "year", "month",
#'                        "day", "doy", "rain", "minimum_temperature", "max_temp"), 
#'                        rename_vars = TRUE, exact_match = FALSE)
#'
data_definitions <- function(data_names,
                             rename_vars = FALSE,
                             exact_match = TRUE) {
  data_definitions <- list()
  
  # Define standard variable names and their corresponding variations ---------------------------------
  # standard variable names
  variable_names <- c("station", "date", "year", "month", "doy", "day", "rain", "tmax", "tmin")
  
  # different accepted variations
  variations <- list(
    station = c("station_name", "name", "station"),
    date = c("date", "Date"),
    year = c("year"),
    month = c("month_abbr", "month"),
    doy = c("DOY", "doy_366", "doy"),
    day = c("day"),
    rain = c("rain", "rainfall", "precipitation"),
    tmax = c("tmax", "max_temperature", "maximum", "max", "temperature_max"),
    tmin = c("tmin", "min_temperature", "minimum", "min", "temperature_min")
  )
  
  # Loop through each variable and its variations
  for (var in variable_names) {
    
    # Check if rename_vars is TRUE, and rename variables accordingly
    if (rename_vars) {
      data_definitions[[var]] <- var
    } else {
      variations_pattern <- paste(variations[[var]], collapse = "|")
      
      if (exact_match){
        pattern_parts <- unlist(strsplit(variations_pattern, "\\|"))
        matches <- data_names[data_names %in% pattern_parts]
      } else {
        # Check if any variable name matches the pattern
        matches <- data_names[grepl(variations_pattern, data_names)]
        # Get the variable names that match the pattern
        if (length(matches) > 1){
          pattern_parts <- unlist(strsplit(variations_pattern, "\\|"))
          matches1 <- data_names[data_names %in% pattern_parts]
          
          if (length(matches1) == 0){
            warning_msg <- warning("multiple matches in variable names. Setting ", matches[1], " as ", var, ".")
            warning(warning_msg)
            matches <- matches[1]
          } else {
            matches <- matches1
          }
        }
      }
      data_definitions[[var]] <- matches
    }
  }
  return(data_definitions)
}