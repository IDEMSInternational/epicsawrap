#' Get Processed Station Metadata
#'
#' This function retrieves and processes station metadata for the specified country and format. The station metadata includes information about station IDs and their associated definitions.
#'
#' @param country A character vector specifying the country code for which station metadata should be retrieved and processed. Options are defined in `get_bucket_name()` (e.g., `"zm"`, `"mw"`).
#' @param station_id A character vector specifying the station ID(s) for the given country.
#' @param format A character vector indicating the desired format of the processed data. It can be "wide", "long", "nested", or "list".
#'
#' @return Depending on the specified format, the function returns the processed station metadata in either wide, long, nested, or list format.
#'
#' @export
#'
#' @examples
#' # Retrieve and process station metadata for country "zm" in wide format
#' #station_metadata_definitions(country = "zm", format = "wide")
#'
#' # Retrieve and process station metadata for countries "zm" and "mw" in long format
#' #station_metadata_definitions(country = c("zm", "mw"), format = "long")
#'
#' # Retrieve and process station metadata for country "zm" in nested format
#' #station_metadata_definitions(country = "zm", format = "nested")
station_metadata_definitions <- function(country, station_id, format = c("wide", "long", "nested", "list")){
  format <- match.arg(format)
  station_data <- station_metadata(country = country, station_id = station_id)
  
  if (format == "list"){
    definitions_id <- lapply(station_data$definitions_id, function(x) x[length(x)])
    result_list <- purrr::map2(.x = station_data$station_id,
                               .y = unlist(definitions_id),
                               .f = ~ c(station_data %>% filter(station_id == .x),
                                       list(data = get_definitions_data(country = country, .x, .y))))
    names(result_list) <- station_data$station_id
    return(result_list)
  } else {
    definitions_id <- lapply(station_data$definitions_id, function(x) x[length(x)])
    definitions_data <- purrr::map2(.x = station_data$station_id,
                                   .y = unlist(definitions_id),
                                   .f = ~ data.frame(station_id = .x, t(unlist(get_definitions_data(country = country, .x, .y)))))
    definitions_data <- dplyr::bind_rows(definitions_data)
    
    wide_df <- dplyr::full_join(station_data, definitions_data)
    
    if (format == "wide"){
      return(wide_df)
    } else {
      long_df <- wide_df %>% tidyr::pivot_longer(cols = !colnames(station_data), names_to = "definition", values_to = "value")
      if(format == "long"){
        return(long_df)
      } else {
        nested_df <- long_df %>% tidyr::nest(.by = colnames(station_data), data = c(definition, value))
        return(nested_df)
      }
    }
  }
}
