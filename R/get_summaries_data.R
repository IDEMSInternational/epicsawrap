#' Get Summaries Data
#'
#' @param country A character vector specifying the country or countries from which to get the data. Options are defined in `get_bucket_name()` (e.g., `"zm"`, `"mw"`).
#' @param station_id A character string specifying the ID of the station for which to get the summary data.
#' @param summary A character string specifying the summary to retrieve.
#'
#' @return A list of data frames containing the summary data for the specified stations and country.
#' @export
#'
#' @examples #
#' 
get_summaries_data <- function(country, station_id, summary) {
  if (length(country) > 1) stop("'country' must be of length 1")
  station_id <- as.character(station_id)
  dfs <- vector("list", length(station_id))
  names(dfs) <- station_id
  bucket_name <- epicsadata:::get_bucket_name(country)
  for (i in seq_along(station_id)) {
    objects_in_space <- googleCloudStorageR::gcs_list_objects(bucket = bucket_name, prefix = paste0("summaries/", summary, "_", station_id[i], "."), versions = TRUE)
    
    if (nrow(objects_in_space) == 0){
      dfs[[i]] <- objects_in_space
      timestamp <- NULL
    } else {
      #   for rds_files > 1 (e.g., if several summary files)
      rds_files <- objects_in_space$name
      
      if (length(rds_files) > 1) {
        timestamps <- gsub(".*\\.(\\d+)\\.rds", "\\1", rds_files)
        timestamps <- suppressWarnings(as.numeric(timestamps))
        most_recent_index <- which.max(timestamps)
        rds_files <- rds_files[most_recent_index]
      }
        station_id[i] <- stringr::str_remove(stringr::str_remove(rds_files, paste0("summaries/", summary, "_")), ".rds")
      f <- paste0(country, "/", rds_files)
      if (file.exists(f)) {
        dfs[[i]] <- readRDS(f)
      } else {
        f <- epicsadata:::update_summaries_data(country, station_id[i], summary)
        dfs[[i]] <- f
      }
      timestamp <- gsub(".*\\.(\\d+)\\.rds", "\\1", rds_files)
    }
  }
  return(list(dfs[[i]], timestamp))
}
