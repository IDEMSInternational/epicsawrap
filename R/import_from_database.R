#' Import EPICSA data from a PostgreSQL database
#'
#' Reads one or more tables from a PostgreSQL database; the counterpart of
#' \code{\link{export_to_database}}. Each \code{*} argument is a logical flag
#' that selects whether the corresponding table is read. Assumes an active
#' database connection object created by \code{DBI::dbConnect()}.
#'
#' @param con A DBIConnection object. An active connection to a PostgreSQL
#'   database, as created by \code{DBI::dbConnect()}.
#' @param station_metadata Logical. Read the \code{station} table? Default
#'   \code{FALSE}.
#' @param definition_data Logical. Read the \code{definition} table? Default
#'   \code{FALSE}.
#' @param summary_station_metadata Logical. Read the
#'   \code{summary_station_metadata} table? Default \code{FALSE}.
#' @param summary_data Logical. Read the \code{summary} table? Default
#'   \code{FALSE}.
#' @param crop_data Logical. Read the \code{crop} table? Default \code{FALSE}.
#' @param station_id Character vector or \code{NULL}. If supplied, only rows
#'   with these \code{station_id} values are returned from the tables that have
#'   a \code{station_id} column. Default \code{NULL} (all rows).
#'
#' @return A named list of data frames, with one element per requested table,
#'   named to match the arguments of \code{\link{export_to_database}}
#'   (\code{station_metadata}, \code{definition_data},
#'   \code{summary_station_metadata}, \code{summary_data}, \code{crop_data}).
#'   Prints a confirmation message for each table read.
#'
#' @note Ensure the connection is closed after use with
#'   \code{DBI::dbDisconnect(con)}.
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
#' db_data <- import_from_database(
#'   con              = con,
#'   station_metadata = TRUE,
#'   summary_data     = TRUE,
#'   station_id       = "ST01"
#' )
#' db_data$summary_data
#'
#' DBI::dbDisconnect(con)
#' }
#'
#' @seealso \code{\link{export_to_database}}, \code{\link[DBI]{dbReadTable}},
#'   \code{\link[DBI]{dbConnect}}, \code{\link[DBI]{dbDisconnect}}
#' @export
import_from_database <- function(con,
                                 station_metadata = FALSE,
                                 definition_data = FALSE,
                                 summary_station_metadata = FALSE,
                                 summary_data = FALSE,
                                 crop_data = FALSE,
                                 station_id = NULL){

  # arguments map to tables, mirroring export_to_database
  requested <- c(
    station_metadata         = station_metadata,
    definition_data          = definition_data,
    summary_station_metadata = summary_station_metadata,
    summary_data             = summary_data,
    crop_data                = crop_data
  )
  table_names <- c(
    station_metadata         = "station",
    definition_data          = "definition",
    summary_station_metadata = "summary_station_metadata",
    summary_data             = "summary",
    crop_data                = "crop"
  )

  if (!any(requested)){
    stop("No tables requested. Set at least one of the data arguments to TRUE.")
  }

  # Internal helper: read one table, optionally filtered by station
  read_data <- function(con, table_name) {
    if (!is.null(station_id) &&
        "station_id" %in% DBI::dbListFields(con, table_name)) {
      query <- DBI::sqlInterpolate(
        con,
        paste0("SELECT * FROM ", DBI::dbQuoteIdentifier(con, table_name),
               " WHERE station_id IN (?ids)"),
        ids = DBI::SQL(paste(DBI::dbQuoteString(con, as.character(station_id)),
                             collapse = ", "))
      )
      data <- DBI::dbGetQuery(con, query)
    } else {
      data <- DBI::dbReadTable(con, table_name)
    }
    print(paste0(
      "Successfully read ",
      nrow(data),
      " rows of ",
      table_name,
      " from database"
    ))
    data
  }

  result <- list()
  for (arg in names(requested)[requested]){
    result[[arg]] <- tryCatch({
      read_data(con, table_names[[arg]])
    }, error = function(e) {
      stop("Failed to read ", table_names[[arg]], " data: ", conditionMessage(e))
    })
  }
  result
}
