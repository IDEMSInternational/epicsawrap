#' Generate summary statistics for extreme weather events.
#'
#' This function generates summary statistics for extreme weather events based on given definitions.
#'
#' @param daily A dataframe containing daily weather data.
#' @param definitions A list containing definitions of extreme events.
#' @param summaries Name of the summary to be generated.
#'
#' @return A dataframe containing summary statistics for extreme events.
#'
#' @examples
#' # Example usage:
#' # Generate summary statistics for extreme rain events
#' #rain_summary <- overall_extremes_summaries(daily_data, definitions_list, "extremes_rain")
overall_extremes_summaries <- function(daily, definitions, summaries){
  if (is.null(definitions[[summaries]]$direction)){
    direction <- "greater"
  } else {
    direction <- as.character(definitions[[summaries]]$direction)
  }
  if (is.null(definitions[[summaries]]$type)){
    type <- "greater"
  } else {
    type <- as.character(definitions[[summaries]]$type)
  }
  
  summary_data <- rpicsa::get_extremes(data = daily,
                                       station = data_names$station,
                                       year = data_names$year,
                                       element = if (summaries == "extremes_rain") data_names$rain else if (summaries == "extremes_tmin") data_names$tmin else if (summaries == "extremes_tmax") data_names$tmax else NULL,
                                       type = type,
                                       direction = direction,
                                       value = as.integer(definitions[[summaries]]$value))
  summary_data <- summary_data %>% rename(!!summaries := "count")
  return(summary_data)
}