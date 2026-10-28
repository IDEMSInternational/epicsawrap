#' Import EPICSA data from a PostgreSQL database
#'
#' Reads one or more tables from a PostgreSQL database; the counterpart of
#' \code{\link{export_to_database}}. Each data argument is a logical flag that
#' selects whether the corresponding table is read. Assumes an active database
#' connection object created by \code{DBI::dbConnect()}.
#'
#' The \code{definition}, \code{summary} and \code{crop} tables all carry a
#' \code{definition_id}. Which definition is returned is decided as follows:
#' \enumerate{
#'   \item If \code{definition_id} is given, only rows with those
#'     \code{definition_id}s are returned.
#'   \item Otherwise, if \code{latest = TRUE}, the \code{summary_station_metadata}
#'     table is used to find the most recent \code{definition_id} (by
#'     \code{time_stamp}) for each \code{station_id} and \code{summary_type},
#'     and only rows with those \code{definition_id}s are returned.
#'   \item Otherwise (\code{latest = FALSE}) all definitions are returned.
#' }
#'
#' @param con A DBIConnection object. An active connection to a PostgreSQL
#'   database, as created by \code{DBI::dbConnect()}.
#' @param station_metadata Logical. Read the \code{station} table? Default
#'   \code{FALSE}.
#' @param definition_data Logical. Read the \code{definition} table? Default
#'   \code{FALSE}.
#' @param summary_station_metadata Logical. Read the
#'   \code{summary_station_metadata} table? Default \code{FALSE}. This table is
#'   filtered by \code{station_id} and \code{summary_type} only, so that all
#'   available definitions remain visible.
#' @param summary_data Logical. Read the \code{summary} table? Default
#'   \code{FALSE}.
#' @param crop_data Logical. Read the \code{crop} table? Default \code{FALSE}.
#' @param station_id Character vector or \code{NULL}. If supplied, only rows
#'   with these \code{station_id} values are returned from the tables that have
#'   a \code{station_id} column. Default \code{NULL} (all stations).
#' @param summary_type Character vector or \code{NULL}. If supplied, only these
#'   summary types (as in \code{summary_station_metadata$summary_type}) are
#'   used. Default \code{NULL} (all summary types).
#' @param definition_id Character vector or \code{NULL}. The specific
#'   \code{definition_id}(s) to return. Overrides \code{latest}. Default
#'   \code{NULL}.
#' @param latest Logical. When \code{definition_id} is \code{NULL}, return only
#'   the most recent definition for each station and summary type? Default
#'   \code{TRUE}.
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
#' # Most recent definition for each station and summary type
#' db_data <- import_from_database(
#'   con          = con,
#'   summary_data = TRUE,
#'   crop_data    = TRUE,
#'   station_id   = "CHIPAT01"
#' )
#'
#' # A specific definition
#' db_data <- import_from_database(
#'   con           = con,
#'   summary_data  = TRUE,
#'   definition_id = "dfFlJMjefmbjgR0l"
#' )
#'
#' # Every definition
#' db_data <- import_from_database(con, summary_data = TRUE, latest = FALSE)
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
                                 station_id = NULL,
                                 summary_type = NULL,
                                 definition_id = NULL,
                                 latest = TRUE){

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

  # Internal helper: read a table, keeping rows that match every filter.
  # Filters are a named list (column = values); any filter on a column the
  # table does not have is ignored. A filter with no values matches nothing.
  # `keys` is an optional data frame of combinations (e.g. station_id,
  # summary_type, definition_id); a row must match one whole combination, using
  # only the key columns the table has.
  read_data <- function(con, table_name, filters = list(), keys = NULL) {
    fields <- DBI::dbListFields(con, table_name)
    filters <- filters[!vapply(filters, is.null, logical(1))]
    filters <- filters[names(filters) %in% fields]

    conditions <- vapply(names(filters), function(column) {
      values <- as.character(filters[[column]])
      if (length(values) == 0) return("1 = 0")
      paste0(DBI::dbQuoteIdentifier(con, column), " IN (",
             paste(DBI::dbQuoteString(con, values), collapse = ", "), ")")
    }, character(1))

    key_columns <- intersect(names(keys), fields)
    if (!is.null(keys) && length(key_columns) > 0) {
      key_values <- unique(keys[, key_columns, drop = FALSE])
      key_values[] <- lapply(key_values, as.character)
      if (nrow(key_values) == 0) {
        conditions <- c(conditions, "1 = 0")
      } else {
        quoted_columns <- vapply(key_columns, function(column) {
          as.character(DBI::dbQuoteIdentifier(con, column))
        }, character(1))
        tuples <- vapply(seq_len(nrow(key_values)), function(i) {
          paste0("(", paste(DBI::dbQuoteString(con, unlist(key_values[i, ])),
                            collapse = ", "), ")")
        }, character(1))
        conditions <- c(conditions, paste0(
          "(", paste(quoted_columns, collapse = ", "), ") IN (",
          paste(tuples, collapse = ", "), ")"))
      }
    }

    query <- paste0("SELECT * FROM ", DBI::dbQuoteIdentifier(con, table_name))
    if (length(conditions) > 0) {
      query <- paste0(query, " WHERE ", paste(conditions, collapse = " AND "))
    }
    data <- DBI::dbGetQuery(con, query)

    print(paste0(
      "Successfully read ",
      nrow(data),
      " rows of ",
      table_name,
      " from database"
    ))
    data
  }

  # Work out which definition_id(s) to return from the definition, summary and
  # crop tables. NULL means no restriction.
  needs_definition <- any(requested[c("definition_data", "summary_data", "crop_data")])
  selected_definition_id <- NULL
  selected_keys <- NULL
  if (!is.null(definition_id)) {
    selected_definition_id <- definition_id
  } else if (latest && needs_definition) {
    metadata <- DBI::dbGetQuery(con, "SELECT * FROM summary_station_metadata")
    if (!is.null(station_id)) {
      metadata <- metadata[metadata$station_id %in% station_id, , drop = FALSE]
    }
    if (!is.null(summary_type)) {
      metadata <- metadata[metadata$summary_type %in% summary_type, , drop = FALSE]
    }
    latest_metadata <- metadata %>%
      dplyr::group_by(station_id, summary_type) %>%
      dplyr::slice_max(time_stamp, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup()
    selected_keys <- latest_metadata[, c("station_id", "summary_type", "definition_id")]
    if (nrow(selected_keys) == 0) {
      warning("No matching entries in summary_station_metadata, so no definition ",
              "could be selected.")
    }
  }

  result <- list()
  for (arg in names(requested)[requested]){
    is_definition_table <- arg %in% c("definition_data", "summary_data", "crop_data")
    filters <- switch(
      arg,
      station_metadata         = list(station_id = station_id),
      summary_station_metadata = list(station_id = station_id,
                                      summary_type = summary_type),
      list(station_id = station_id, definition_id = selected_definition_id,
           summary_type = summary_type)
    )
    result[[arg]] <- tryCatch({
      read_data(con, table_names[[arg]], filters,
                keys = if (is_definition_table) selected_keys)
    }, error = function(e) {
      stop("Failed to read ", table_names[[arg]], " data: ", conditionMessage(e))
    })
  }
  result
}
