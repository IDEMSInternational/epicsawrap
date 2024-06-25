#' Import Data from Climsoft
#'
#' Connects to a Climsoft database and imports data based on the specified filters for stations and elements, with options to include observation flags and station information.
#'
#' @param con Connection object to the Climsoft database, default is the result of \code{get_climsoft_conn()}.
#' @param stationfiltercolumn Name of the column to filter by stations, default is 'stationId'.
#' @param stations Vector of station IDs to filter the data, defaults to an empty vector.
#' @param elementfiltercolumn Name of the column to filter by elements, default is 'elementId'.
#' @param elements Vector of element IDs to filter the data, defaults to an empty vector.
#' @param include_observation_flags Boolean, if TRUE includes observation flags in the output, defaults to FALSE.
#' @param include_station_info Boolean, if TRUE includes station metadata in the output, defaults to FALSE.
#' @param unstack_data Boolean. Option to unstack data once read in. 
#' @param start_date Start date for filtering the observations, format should be Date, defaults to NULL.
#' @param end_date End date for filtering the observations, format should be Date, defaults to NULL.
#' 
#' @return A list containing Climsoft station and observation data based on the filters applied. If `include_station_info` is TRUE, the list will have two elements: 'Metadata' with station details and 'Daily data' with observations.
#' 
#' @examples
#' #con <- get_climsoft_conn()
#' #data <- import_from_climsoft(con, stations = c("101", "102"), elements = c("1", "2"), start_date = as.Date("2020-01-01"), end_date = as.Date("2020-01-31"))
#'
#' @export
import_from_climsoft <- function(con = get_climsoft_conn(),
                                 stationfiltercolumn = "stationId",
                                 stations = c(),
                                 elementfiltercolumn = "elementId",
                                 elements = c(),
                                 include_observation_flags = FALSE,
                                 include_station_info = FALSE,
                                 unstack_data = TRUE,
                                 start_date = NULL,
                                 end_date = NULL) {
  con <- con # get connection
  
  #get stations database data and station ids values
  if (length(stations) > 0) {
    #construct a string of station values from the passed station vector eg of result ('191','122')
    passed_station_values <- paste0("(", paste0("'", stations, "'", collapse =  ", "), ")")
    
    #get the station info of the passed station values
    db_station_info <- DBI::dbGetQuery(con, paste0( "SELECT * FROM station WHERE ", stationfiltercolumn, " IN ", passed_station_values,  ";"))
    
    #set values of station ids only
    if (stationfiltercolumn == "stationId") {
      station_ids_values <- passed_station_values
    } else{
      station_ids_values <- paste0("(", paste0("'", db_station_info$stationId, "'", collapse = ", "),")")
    }
  }
  
  #if there are no elements passed then stop and throw error
  if (length(elements) < 1) stop("start_date must be of type Date.")
  
  #set values of element ids only
  if (elementfiltercolumn == "elementId") {
    #get element id values directly from passed data
    element_ids_values <- paste0("(", paste0(elements, collapse = ", "), ")")
  } else{
    #get element id values from the database
    passed_element_values <- paste0("(", paste0("'", elements, "'", collapse = ", "), ")")
    db_elements_ids <- DBI::dbGetQuery( con, paste0("SELECT elementId FROM obselement WHERE ", elementfiltercolumn,  " IN ",  passed_element_values, ";" ))
    element_ids_values <- paste0("(", paste0(sprintf("%d", db_elements_ids$elementId), collapse = ", "), ")")
  }
  
  # if(include_elements_info) {
  #   db_elements_info <- DBI::dbGetQuery(con, paste0("SELECT elementId, elementName, abbreviation, description, elementtype, upperLimit, lowerLimit, units FROM obselement WHERE elementId ", " IN ", element_ids_values, ";" ))
  # }
  
  flags_column_col_sql <- " "
  if (include_observation_flags) {
    flags_column_col_sql <- ", observationfinal.flag AS flag"
  }
  
  #get databounds filter query if dates have been passed
  date_bounds_filter <- ""
  if (!is.null(start_date)) {
    if (!lubridate::is.Date(start_date))
      stop("start_date must be of type Date.")
    start_date <- format(start_date, format = "%Y-%m-%d")
    date_bounds_filter = paste0(date_bounds_filter, " AND obsDatetime >= ", sQuote(start_date))
  }
  if (!is.null(end_date)) {
    if (!lubridate::is.Date(end_date))
      stop("end_date must be of type Date.")
    end_date <- format(end_date, format = "%Y-%m-%d")
    date_bounds_filter <- paste0(date_bounds_filter," AND obsDatetime <=", sQuote(end_date))
  }
  
  #construct observation data sql query and get data from database
  if (length(stations) > 0) {
    #if stations passed get observation data of selected elements of passed stations
    db_observation_data <- DBI::dbGetQuery(con, paste0("SELECT observationfinal.recordedFrom As station, obselement.abbreviation AS element, observationfinal.obsDatetime AS datetime, observationfinal.obsValue AS obsvalue", flags_column_col_sql, " FROM observationfinal INNER JOIN obselement ON observationfinal.describedBy = obselement.elementId WHERE observationfinal.recordedFrom IN ", station_ids_values, " AND observationfinal.describedBy IN ", element_ids_values, date_bounds_filter, " ORDER BY observationfinal.recordedFrom, observationfinal.describedBy;"))
  } else{
    #if stations have not been passed get observation data of passed elements of all stations
    db_observation_data <- DBI::dbGetQuery(con, paste0("SELECT observationfinal.recordedFrom As station, obselement.abbreviation AS element, observationfinal.obsDatetime AS datetime, observationfinal.obsValue AS obsvalue", flags_column_col_sql, " FROM observationfinal INNER JOIN obselement ON observationfinal.describedBy = obselement.elementId WHERE observationfinal.describedBy IN ", element_ids_values, date_bounds_filter, " ORDER BY observationfinal.recordedFrom, observationfinal.describedBy;"))
    
    #then get the stations ids (uniquely) from the observation data and use the ids to get station info
    station_ids_values <- paste0("(", paste0("'", as.character(unique(db_observation_data$station) ), "'", collapse = ", "), ")")
    db_station_info <- DBI::dbGetQuery(con, paste0("SELECT * FROM station WHERE stationId IN ", station_ids_values, ";" ))
  }

  if(unstack_data){
    db_observation_data <- tidyr::pivot_wider(db_observation_data,
                                                     names_from = element,
                                                     values_from = obsvalue)
  }
  if (include_station_info) {
    data_list <- list(db_station_info, db_observation_data)
    names(data_list) <- c("Metadata", "Daily data")
  } else {
    data_list <- db_observation_data
  }
  return(data_list)
}