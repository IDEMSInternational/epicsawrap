#' Update Spells
#'
#' @param data_frame A data frame containing the columns specified in `data_names`.
#' @param data_names A list of column names that are needed from the `data_frame`.
#' @param summary_data A data frame of the computed summaries.
#' @param definitions A list containing definitions to be read in.
#' @param data_book The data book object where the data object is stored.
#' @param element Determines the specific variant of the definitions to use.
#' 
#' @return A data frame in the data book at the year (and station) level containing spells data.
#' @export

update_spells <- function(data_frame, data_names, summary_data_frame, definitions, data_book,
                          element = c("longest_rain_spell", "longest_tmin_spell", "longest_tmax_spell")){
    element <- match.arg(element)
    spells_definitions <- dplyr::case_match(
        element,
        "longest_rain_spell" ~ definitions$annual_summaries$longest_rain_spell,
        "longest_tmin_spell" ~ definitions$annual_summaries$longest_tmin_spell,
        "longest_tmax_spell" ~ definitions$annual_summaries$longest_tmax_spell
    )
    
    direction = as.character(spells_definitions$direction)
    value = as.numeric(spells_definitions$value)
    lb_value = as.numeric(spells_definitions$value_lb)
    day_from = as.numeric(spells_definitions$start_day)
    day_to = as.numeric(spells_definitions$end_day)
    return_max_spell = as.logical(spells_definitions$return_max_spell)
    return_all_spells = as.logical(spells_definitions$return_all_spells)
    s_start_doy = as.numeric(spells_definitions$s_start_doy)
    s_start_month = s_start_month <- as.numeric(format(as.Date(s_start_doy, origin = "2020-12-31"), "%m"))
    
    get_spells_data <- rpicsa::get_spells_data(data = data_frame,
                                               date_time = data_names$date,
                                               year = data_names$year,
                                               station = data_names$station,
                                               element = data_names$element,
                                               doy = data_names$doy,
                                               summary_data = summary_data_frame,
                                               day_from = day_from,
                                               day_to = day_to,
                                               s_start_month = 1, # this was not in the definitions file. We had s_start_doy instead. computed it as was done in update_start_rains
                                               direction = direction,
                                               value = value,
                                               lb_value = lb_value,
                                               return_max_spell = return_max_spell,
                                               return_all_spells = return_all_spells,
                                               data_book = data_book)
}