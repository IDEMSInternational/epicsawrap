#' Date Amendments
#'
#' This function performs date amendments based on the provided parameters.
#' It adjusts the year, day of year (DOY), and calculates new date values.
#'
#' @param summary A character string specifying the summary type.
#' @param data A data frame containing the original data.
#' @param variable A character string specifying the variable name in the data frame.
#' @param definitions A list containing definitions for summary types.
#'
#' @return The modified data frame with adjusted dates and day of year values.
#'
#' @export
#'
#' @examples
#' summary <- "example_summary"
#' data <- data.frame(year = c(2023, 2024, 2025), variable = c(1, 200, 366))
#' variable <- "variable"
#' definitions <- list(example_summary = list(s_start_doy = 1))
#' date_amendments(summary, data, variable, definitions)
date_amendments <- function(summary, data, variable, definitions){
  
  #' if shift year, then do this
  #' 
  # Warn about using initial year as 'year' for summary
  
  # Set origin for date conversion
  origin <- "2024-01-01"
  
  if (!is.null(definitions[[summary]]$s_start_doy)){
    warning(paste0("Giving initial year as 'year' for ", summary))
    data$year <- as.integer(sub("-.*", "", data$year))
    
    # Shift doy output
    s_start_doy <- definitions[[summary]]$s_start_doy
    
    data <- data %>%
      dplyr::mutate(wrapped_doy = {{ variable }} + s_start_doy - 1,
                    dplyr::across({{ variable }}, ~ ({{ variable }} + s_start_doy - 1) %% 366),
                    dplyr::across({{ variable }}, ~ ifelse({{ variable }} == 0, 366, {{ variable }})),
                    initial_date = as.Date({{ variable }}, origin = as.Date(-1, origin = origin)),
                    initial_day = lubridate::day(initial_date),
                    initial_month = lubridate::month(initial_date),
                    initial_year = ifelse(wrapped_doy > 366, year + 1, year),
                    !!paste0(as.character(ensym(variable)), "_date") := as.Date(paste(initial_year,
                                                                                      initial_month,
                                                                                      initial_day,
                                                                                      sep="-"),
                                                                                "%Y-%m-%d")) %>%
      dplyr::select(-c(initial_date, initial_day, initial_month, initial_year, wrapped_doy))
    
  } else {
    s_start_doy <- 1
    data <- data %>%
      dplyr::mutate(initial_date = as.Date({{ variable }}, origin = as.Date(-1, origin = origin)),
                    initial_day = lubridate::day(initial_date),
                    initial_month = lubridate::month(initial_date),
                    initial_year = ifelse({{ variable }} > 366, year + 1, year),
                    !!paste0(as.character(ensym(variable)), "_date") := as.Date(paste(initial_year,
                                                                                      initial_month,
                                                                                      initial_day,
                                                                                      sep="-"),
                                                                                "%Y-%m-%d")) %>%
      dplyr::select(-c(initial_date, initial_day, initial_month, initial_year))
  }
  
  # always give date and doy output
  

  
  # Convert to date and create new column with variable name + "_date"

  # else if DOY isn't 366-type
  # start_rains <- start_rains %>%
  #   dplyr::mutate(start_rains_date = as.Date(start_rains,
  #                                     origin = as.Date(s_start_doy,
  #                                                      origin = paste0(year, "-01-01"))))
  #}
  
  return(data)
}