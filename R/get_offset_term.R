#' Get Offset Term
#'
#' This function retrieves the start day of the year (DOY) from the metadata of the given data.
#' If there are multiple start DOYs, it issues a warning and selects the first one.
#'
#' @param data A data object from which to retrieve the start DOY. This data object is expected 
#' to have an associated metadata containing a `doy_start` field.
#' @return Returns a single start DOY value if found in the metadata; otherwise, returns `NULL`.
#' @importFrom stats na.omit
#' @examples
#' #data <- some_function_to_get_data()
#' #offset_term <- get_offset_term(data)
#' #print(offset_term)
#' @export
get_offset_term <- function(data){
  column_metadata <- data_book$get_variables_metadata(data)
  if (!is.null(column_metadata$doy_start)) {
    s_start_doy <- unique(na.omit(column_metadata$doy_start))
    if (length(s_start_doy) > 1) {
      warning(paste0("Multiple start DOYs. Taking ", s_start_doy[1]))
      s_start_doy <- s_start_doy[1]
    }
  } 
  return(s_start_doy)
}