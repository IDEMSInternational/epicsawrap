#' Export EPICSA data to a PostgreSQL database
#'
#' Appends one or more data frames to their respective tables in a PostgreSQL
#' database. Each parameter is optional; only non-\code{NULL} arguments are
#' written. Assumes an active database connection object created by
#' \code{DBI::dbConnect()}.
#'
#' @param con A DBIConnection object. An active connection to a PostgreSQL
#'   database, as created by \code{DBI::dbConnect()}.
#' @param station_metadata Data frame or \code{NULL}. Station metadata to
#'   append to the \code{station} table. Default \code{NULL}.
#' @param definition_data Data frame or \code{NULL}. Definition data to append
#'   to the \code{definition} table. Default \code{NULL}.
#' @param summary_station_metadata Data frame or \code{NULL}. Summary-level
#'   station metadata to append to the \code{summary_station_metadata} table.
#'   All stations must already exist in the database before writing. Default
#'   \code{NULL}.
#' @param summary_data Data frame or \code{NULL}. Summary data to append to
#'   the \code{summary} table. Default \code{NULL}.
#' @param crop_data Data frame or \code{NULL}. Crop data to append to the
#'   \code{crop} table. Default \code{NULL}.
#'
#' @return Called primarily for its side effect of writing to the database.
#'   Prints a confirmation message for each table successfully written.
#'
#' @note Target tables will be created automatically if they do not already
#'   exist. All data is appended to existing rows. Ensure the connection is
#'   closed after use with \code{DBI::dbDisconnect(con)}.
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(
#'   RPostgres::Postgres(),
#'   host     = "your_host",
#'   dbname   = "your_dbname",
#'   user     = "your_user",
#'   password = "your_password",
#'   port     = 5432
#' )
#'
#' export_to_database(
#'   con              = con,
#'   station_metadata = my_stations,
#'   summary_data     = my_summary
#' )
#'
#' DBI::dbDisconnect(con)
#' }
#'
#' @seealso \code{\link[DBI]{dbWriteTable}}, \code{\link[DBI]{dbConnect}},
#'   \code{\link[DBI]{dbDisconnect}}
#' @export
export_to_database <- function(con,
                               station_metadata = NULL,
                               definition_data = NULL,
                               summary_station_metadata = NULL,
                               summary_data = NULL,
                               crop_data = NULL){
  
  # 1. send up station metadata 
  if (!is.null(station_metadata)){
    dbWriteTable(
      conn      = con,
      name      = "station",          # table name in postgres
      value     = station_metadata,
      append    = TRUE,
      row.names = FALSE
    )
    print("Successfully sent station metadata to database")
  }
  
  # 2. send up definition 
  if (!is.null(definition_data)){
    dbWriteTable(
      conn      = con,
      name      = "definition",          # table name in postgres
      value     = definition_data,
      append    = TRUE,               # append to existing table
      row.names = FALSE
    )
    print("Successfully sent definition data to database")
  }
  
  # 3. send summary station metadata
  if (!is.null(summary_station_metadata)){
    
    # fetch existing stations from db and check all stations in
    # summary_station_metadata are present before writing
    imported_station_from_db <- DBI::dbReadTable(conn = con, name = "station")
    
    missing_stations <- setdiff(
      summary_station_metadata$station_id,
      imported_station_from_db$station_id
    )
    
    if (length(missing_stations) > 0){
      stop(
        "The following station_id(s) in summary_station_metadata are not present ",
        "in the 'station' table: ",
        paste(missing_stations, collapse = ", "),
        ". Update station metadata to include missing stations."
      )
    }
    
    dbWriteTable(
      conn      = con,
      name      = "summary_station_metadata",
      value     = summary_station_metadata,
      append    = TRUE,
      row.names = FALSE
    )
    print("Successfully sent summary station metadata to database")
  }
  
  # TODO: do we need to do a check for stations in the summary and crop data frames?
  
  # internal helper: write a large data frame in chunks
  write_in_chunks <- function(con, data, table_name, chunk_size = 100) {
    for (i in seq(1, nrow(data), by = chunk_size)) {
      chunk <- data[i:min(i + chunk_size - 1, nrow(data)), ]
      DBI::dbWriteTable(conn      = con,
                        name      = table_name,
                        value     = chunk,
                        append    = TRUE,
                        row.names = FALSE)
      cat("Sent rows", i, "to", min(i + chunk_size - 1, nrow(data)),
          "of", nrow(data), "\n")
    }
  }
  
  # 4. send summary_data
  if (!is.null(summary_data)){
    tryCatch({
      write_in_chunks(con, summary_data, "summary")
      print("Successfully sent summary data to database")
    }, error = function(e) {
      stop("Failed to write summary data: ", conditionMessage(e))
    })
  }
  
  # 5. send crop data
  if (!is.null(crop_data)){
    tryCatch({
      write_in_chunks(con, crop_data, "crop")
      print("Successfully sent crop data to database")
    }, error = function(e) {
      stop("Failed to write crop data: ", conditionMessage(e))
    })
  }
}

#' Collate and standardise station metadata
#'
#' Selects and renames columns from a raw station metadata data frame into a
#' standardised format ready for export to the database. Adds a UTC timestamp
#' and sets a default \code{status} of \code{"active"} for all stations.
#'
#' @param data Data frame. The raw station metadata to process.
#' @param station_id Character or \code{NULL}. Name of the column in \code{data}
#'   containing station IDs. Default \code{NULL}.
#' @param station_name Character or \code{NULL}. Name of the column in \code{data}
#'   containing station names. Default \code{NULL}.
#' @param longitude Character or \code{NULL}. Name of the column in \code{data}
#'   containing longitude values. Default \code{NULL}.
#' @param latitude Character or \code{NULL}. Name of the column in \code{data}
#'   containing latitude values. Default \code{NULL}.
#' @param elevation Character or \code{NULL}. Name of the column in \code{data}
#'   containing elevation values. Default \code{NULL}.
#' @param district Character or \code{NULL}. Name of the column in \code{data}
#'   containing district names. Default \code{NULL}.
#' @param country_code Character or \code{NULL}. Name of the column in \code{data}
#'   containing country codes. Default \code{NULL}.
#'
#' @return A data frame containing only the selected columns, renamed to the
#'   standardised names, with two additional columns: \code{time_stamp} (UTC
#'   datetime of when the function was called) and \code{status} (set to
#'   \code{"active"} for all rows).
#'
#' @note Only columns with a non-\code{NULL} mapping are selected and renamed.
#'   Any columns in \code{data} not referenced by the parameters are dropped.
#'   The output is intended to be passed to \code{export_to_database()} as the
#'   \code{station_metadata} argument.
#'
#' @examples
#' raw <- data.frame(
#'   id    = c("ST01", "ST02"),
#'   name  = c("Station One", "Station Two"),
#'   lon   = c(32.1, 33.4),
#'   lat   = c(-1.2, -2.5),
#'   elev  = c(1100, 950),
#'   dist  = c("Northern", "Southern"),
#'   ccode = c("KE", "KE")
#' )
#'
#' collate_station_metadata(
#'   data         = raw,
#'   station_id   = "id",
#'   station_name = "name",
#'   longitude    = "lon",
#'   latitude     = "lat",
#'   elevation    = "elev",
#'   district     = "dist",
#'   country_code = "ccode"
#' )
#'
#' @seealso \code{\link{export_to_database}}
#' @export
collate_station_metadata <- function(data,
                                     station_id = NULL,
                                     station_name = NULL,
                                     longitude = NULL,
                                     latitude = NULL,
                                     elevation = NULL,
                                     district = NULL,
                                     country_code = NULL) {
  
  # Creates a timestamp
  time_stamp <- lubridate::now(tz = 'UTC')
  
  # rename variables
  rename_cols <- c(
    station_id = station_id,
    station_name = station_name,
    longitude = longitude,
    latitude = latitude,
    elevation = elevation,
    district = district,
    country_code = country_code
  )
  rename_cols <- rename_cols[!sapply(rename_cols, is.null)]
  data <-   data %>%
    dplyr::select(dplyr::any_of(c(station_id, station_name, longitude,
                                  latitude, elevation, district, country_code))) %>%
    dplyr::mutate(time_stamp = time_stamp,
                  status = "active") %>%
    dplyr::rename(!!!stats::setNames(rlang::syms(rename_cols), names(rename_cols)))
  return(data)
}