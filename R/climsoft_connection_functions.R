#' Set Climsoft Connection
#'
#' Establishes a connection to a Climsoft database and stores it in a package environment for later use.
#'
#' @param dbname Name of the database.
#' @param user Username for database access.
#' @param password Password for database access.
#' @param host Host where the database server is located.
#' @param port Port number on which the database server is running.
#' 
#' @return Invisible. The function does not return anything but stores the connection in a designated package environment.
#' 
#' @examples
#' #set_climsoft_conn("climsoft_db", "user", "password", "localhost", "3306")
#'
#' @importFrom DBI dbConnect
#' @importFrom RMySQL MySQL
#' @export
set_climsoft_conn <- function(dbname, user, password, host, port){
  conn <- DBI::dbConnect(drv = RMySQL::MySQL(), dbname = dbname,
                         user = user, password = password, host = host, port = port)
  pkg_env$conn <- conn
}

#' Get Climsoft Connection
#'
#' Retrieves the stored Climsoft database connection from the package environment.
#'
#' @return The database connection object.
#'
#' @examples
#' #con <- get_climsoft_conn()
#'
#' @export
get_climsoft_conn <- function(){
  get("conn", envir = pkg_env)
}
