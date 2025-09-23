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

#' Update Metadata Definition ID
#'
#' This function updates the definitions ID in the station metadata for a given country and station ID. 
#' It can either overwrite the existing definitions ID or append a new one.
#'
#' @param country A character string representing the country code.
#' @param station_id A character string representing the station ID.
#' @param definition_id A character string representing the new definition ID to be added.
#' @param overwrite A logical value indicating whether to overwrite the existing definitions ID. 
#' @param add_climsoft A logical value indicating whether to add in climsoft details. This will add in the values from `elementfiltercolumn` and `elements`.
#' @param elementfiltercolumn Name of the column to filter by elements, default is 'elementName'.
#' @param elements Vector of element IDs to filter the data.
#' If \code{TRUE}, the existing definitions ID will be overwritten. If \code{FALSE}, the new 
#' definition ID will be appended. Default is \code{FALSE}.
#'
#' @return None. The function updates the metadata in the specified cloud storage bucket.
#' 
#' @export
update_metadata_definition_id <- function(country, station_id, definition_id, overwrite = FALSE,
                                          add_climsoft = FALSE,
                                          elementfiltercolumn = "elementName",
                                          elements = c("Temp  Daily Max", "Temp  Daily Min", "Precip  Daily")) {
  bucket <- get_bucket_name(country)
  station_id_names <- station_id
  complete_metadata_from_bucket <- get_station_metadata(country)
  reference <- complete_metadata_from_bucket$station_id
  complete_metadata_from_bucket_filt <- complete_metadata_from_bucket %>% dplyr::filter(station_id %in% station_id_names)
  if (nrow(complete_metadata_from_bucket_filt) > 0){
    # add definition ID
    complete_metadata_from_bucket_rest <- complete_metadata_from_bucket %>% dplyr::filter(!station_id %in% station_id_names)
    if (!is.list(complete_metadata_from_bucket_rest$definitions_id)) complete_metadata_from_bucket_rest$definitions_id <- as.list(complete_metadata_from_bucket_rest$definitions_id)
    if (overwrite){
      complete_metadata_from_bucket_filt$definitions_id <- definition_id
    } else {
      complete_metadata_from_bucket_filt$definitions_id <- purrr::map(.x = complete_metadata_from_bucket_filt$definitions_id, .f = ~ unique(c(.x, definition_id)))
    }
    complete_metadata_from_bucket <- dplyr::bind_rows(complete_metadata_from_bucket_rest, complete_metadata_from_bucket_filt)
    complete_metadata_from_bucket <- complete_metadata_from_bucket[order(match(complete_metadata_from_bucket$station_id, reference)),]
  } else {
    new_df <- data.frame(station_id = station_id_names)
    new_df$definitions_id <- purrr::map(.x = definition_id, .f = ~ unique(c(.x)))
    if (add_climsoft){
      climsoft_list <- list(elementfiltercolumn, elements)
      names(climsoft_list) <- c("elementName", "elements")
      new_df$climsoft_list <- list(climsoft_list)
    }
    complete_metadata_from_bucket <- dplyr::bind_rows(complete_metadata_from_bucket, new_df)
  }
  object_function <- function(input, output) { saveRDS(input, file = output) }
  googleCloudStorageR::gcs_upload(file = complete_metadata_from_bucket, bucket = bucket, name = "metadata.rds", object_function = object_function, predefinedAcl = "bucketLevel")
}