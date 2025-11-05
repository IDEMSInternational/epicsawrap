#' Update Spells
#'
#' @param data_frame            A data frame (or name) containing daily data.
#' @param data_names            A list with element names for daily data:
#'   \code{date}, \code{year}, \code{station}, \code{doy}, \code{rain}.
#' @param seasonal_data_frame   A data frame (or name) containing seasonal windows.
#' @param seasonal_data_names   A list with element names for seasonal data:
#'   \code{start_day}, \code{end_day}. If these are not given, then values from
#'   the definitions file are used.
#' @param definitions           A list with (optional) elements
#'   \code{$annual_summaries$longest_rain_spell} or
#'   \code{$annual_summaries$longest_tmin_spell} or
#'   \code{$annual_summaries$longest_tmax_spell}.
#' @param data_book             The data book object forwarded to
#'   \code{rpicsa::crops_definitions()}.
#' 
#' @return A data frame in the data book at the year (and station) level containing spells data.
#' @export

update_spells <- function(data_frame, data_names, summary_data_frame = NULL,
                          seasonal_data_names = NULL, definitions, data_book,
                          element = c("longest_rain_spell", "longest_tmin_spell", "longest_tmax_spell")){
    element <- match.arg(element)
    
    spells_definitions <- switch(
        element,
        longest_rain_spell = definitions$annual_summaries$longest_rain_spell,
        longest_tmin_spell = definitions$annual_summaries$longest_tmin_spell,
        longest_tmax_spell = definitions$annual_summaries$longest_tmax_spell,
        stop("Unknown spell length definition: ", element)
    )
    
    direction = as.character(spells_definitions$direction)
    value = as.numeric(spells_definitions$value)
    lb_value = as.numeric(spells_definitions$value_lb)
    return_max_spell = as.logical(spells_definitions$return_max_spell)
    return_all_spells = as.logical(spells_definitions$return_all_spells)
    s_start_doy = as.numeric(spells_definitions$s_start_doy)
    s_start_month = s_start_month <- as.numeric(format(as.Date(s_start_doy, origin = "2020-12-31"), "%m"))
    
    if (!is.null(seasonal_data_names)){
      day_from <- seasonal_data_names$day_from
      day_to <- seasonal_data_names$day_to 
    } else {
      day_from <- NULL
      day_to <- NULL
    }
    if (is.null(day_from)) day_from <- as.numeric(spells_definitions$start_day)
    if (is.null(day_to)) day_to <- as.numeric(spells_definitions$end_day)
    
    get_spells_data <- rpicsa::get_spells_data(data = data_frame,
                                               date_time = data_names$date,
                                               year = data_names$year,
                                               station = data_names$station,
                                               element = data_names$element,
                                               doy = data_names$doy,
                                               summary_data = summary_data_frame,
                                               day_from = day_from,
                                               day_to = day_to,
                                               s_start_month = s_start_month,
                                               direction = direction,
                                               value = value,
                                               lb_value = lb_value,
                                               return_max_spell = return_max_spell,
                                               return_all_spells = return_all_spells,
                                               data_book = data_book)
}