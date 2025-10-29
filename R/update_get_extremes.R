#' Update Get Extremes 
#'
#' @param data_frame A data frame containing the columns specified in `data_names`.
#' @param data_names A list of column names that are needed from the `data_frame`.
#' @param definitions A list containing definitions to be read in.
#' @param data_book The data book object where the data object is stored.
#'
#' @return A data frame in the data book at the year (and station) level containing extremes data.
#' @export
#'
update_get_extremes <- function(data_frame, data_names, definitions, data_book){
    get_extremes_definitions <- definitions$annual_summaries$extreme_rain

    sign <- as.character(get_extremes_definitions$sign)
    direction <- dplyr::case_match(
        sign,
        ">=" ~ "greater",
        "<=" ~ "less" # I don't know how the between and outer might look like, hence their omission. Will it be like [">=", "<="]?
    )
    threshold <- as.numeric(get_extremes_definitions$threshold)
    value <- ifelse(direction %in% c("greater", "less"), threshold, 0.85) # if direction is neither greater nor less, then value is set to its default.
    
    get_extremes <- rpicsa::get_extremes(data = data_frame,
                                         element = data_names$rain, #  I made it rain specifically because I used the $extreme_rain definitions.
                                         date_time = data_names$date,
                                         year = data_names$year,
                                         station = data_names$station,
                                         direction = direction,
                                         s_start_month = 1, # No value in definitions. Used default.
                                         value = value,
                                         lb_value = 0, # No value in definitions. Used default.
                                         na_rm = FALSE, # No value in definitions. Used default.
                                         na_prop = NULL, # No value in definitions. Used default.
                                         na_n = NULL, # No value in definitions. Used default.
                                         na_consec = NULL, # No value in definitions. Used default.
                                         na_n_non = NULL, # No value in definitions. Used default.
                                         data_book = data_book)
    
}