#' Get R-Instat definitions
#'
#' Retrieves R-Instat definitions based on given calculations.
#'
#' @param calculation A list of calculations.
#' 
#' @return A list of R-Instat definitions.
#' 
#' @examples
#' # Example usage:
#' #get_r_instat_definitions()
get_r_instat_definitions <- function(calculation){
  manips <- NULL
  type <- NULL
  if (length(calculation) > 0){
    for (i in 1:length(calculation)){
      calc <- calculation[[i]]
      type[i] <- calc$type
      if (type[i] == "summary"){
        # this will tell us if it is DOY or date (or both)
        # want recursive here:
        # run the function with 
        manips[[i]] <- c(variables = calc$calculated_from,
                         function_exp = calc$function_exp, 
                         get_r_instat_definitions(calculation = calc$manipulations))
        type[i] <- calc$result_name
      } else if (type[i] == "by"){
        manips[[i]] <- calc$calculated_from
        type[i] <- paste0("by_", i)
      } else if (type[i] == "filter"){
        if (length(calc$sub_calculations) > 0){
          manips[[i]] <- c(calc$function_exp, get_r_instat_definitions(calculation = calc$sub_calculations))
          type[i] <- paste0("filter")
        } else {
          manips[[i]] <- calc$function_exp
          type[i] <- paste0("filter_2")
        }
      } else if (type[i] == "calculation"){
        if (length(calc$sub_calculations) > 0){
          manips[[i]] <- c(calc$function_exp, get_r_instat_definitions(calculation = calc$sub_calculations))
          type[i] <- calc$result_name
        } else {
          if (length(calc$calculated_from) > 0){
            manips[[i]] <- c(calc$calculated_from, calc$function_exp)        
          } else {
            manips[[i]] <- calc$function_exp
          }
          type[i] <- calc$result_name
        }
        type[i] <- calc$result_name
      }
    }
    if (!is.null(manips)){
      names(manips) <- type
    }
    return(manips)
  }
}

#' Get Offset Term
#'
#' This function retrieves the start day of the year (DOY) from the metadata of the given data.
#' If there are multiple start DOYs, it issues a warning and selects the first one.
#'
#' @param data_by_year A data object from which to retrieve the start DOY. This data object is expected 
#' to have an associated metadata containing a `doy_start` field.
#' @return Returns a single start DOY value if found in the metadata; otherwise, returns `NULL`.
#' @importFrom stats na.omit
#' @examples
#' #data <- some_function_to_get_data()
#' #offset_term <- get_offset_term(data)
#' #print(offset_term)
get_offset_term <- function(data_by_year){
  column_metadata <- data_book$get_variables_metadata(data_by_year)
  if (!is.null(column_metadata$doy_start)) {
    s_start_doy <- unique(na.omit(column_metadata$doy_start))
    if (length(s_start_doy) > 1) {
      warning(paste0("Multiple start DOYs. Taking ", s_start_doy[1]))
      s_start_doy <- s_start_doy[1]
    }
    return(s_start_doy)
  } else {
    return(1)
  } 
}