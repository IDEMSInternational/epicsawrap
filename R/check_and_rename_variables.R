#' Check and Rename Variables in a Dataset
#'
#' This function checks the variable names in a dataset and renames them based on
#' the provided `data_names` vector.
#'
#' @param data A data frame or data table containing the dataset.
#' @param data_names A named character vector where the names are the standard variable names,
#'        and the values are the corresponding variable names in the dataset.
#'
#' @return A data frame with variable names renamed according to `data_names`.
#'
#' @examples
#' 
#' # Example: Check and rename variables in a dataset
#' data <- data.frame(station_name = c("A", "B", "C"), date = c("2022-01-01", "2022-01-02", "2022-01-03"), 
#'                    tmax = c(25, 26, 24), tmin = c(15, 16, 14))
#' data_names <- c(station = "station_name", date = "date", tmax = "tmax", tmin = "tmin")
#' renamed_data <- check_and_rename_variables(data, data_names)
#'
#' @export
#' 
check_and_rename_variables <- function(data, data_names) {
  
  # Check if all variable names in data_names exist in the dataset
  missing_vars <- setdiff(names(data_names), names(data))
  missing_names <- setdiff(names(data), names(data_names))
  
  # Rename if variables are missing in the list of data_names
  if (length(missing_vars) > 0) {
    variations <- list(station = c("station_name", "name","station"),
                       date = c("date", "Date"), 
                       year = c("year"),
                       month = c("month_abbr", "month"),
                       doy = c("DOY", "doy_366", "doy"),
                       day = c("day"),
                       rain = c("rain", "rainfall", "precipitation", "PRECIP"),
                       tmax = c("tmax", "max_temperature", "maximum", "max", "temperature_max", "TMPMAX"),
                       tmin = c("tmin","min_temperature", "minimum", "min", "temperature_min", "TMPMIN"))
    
    # Loop through the missing variable names
    
    for (var in missing_vars) {
      variations_pattern <- paste(variations[[var]], collapse = "|")
      
      # see potential matches that could be "station" (or whatever the variable is)
      matches <- missing_names[grepl(variations_pattern, missing_names)]
      
      # now how many are there?
      if (length(matches) < 1){
        warning(paste0("No matches found for ", var))
      } else {
        if (length(matches) > 1) {
          pattern_parts <- unlist(strsplit(variations_pattern, "\\|"))
          matches1 <- matches[matches %in% pattern_parts]
          if (length(matches1) == 0) {
            warning("multiple matches in variable names for ", paste(matches, collapse = ", "), ".\n Setting ", matches[1], " as ", var, ".")
            matches <- matches[1]
          } else {
            matches <- matches1
          }
        }
        colnames(data)[colnames(data) == matches] <- var
        cat("Renamed", matches, "to", var, "\n")
      }
    }
    
  }
  return(data)
}
