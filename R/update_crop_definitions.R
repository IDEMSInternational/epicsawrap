#' Get values from a vector or a {from, to, by} list
#'
#' Convenience extractor for definition entries that may be stored either as a
#' numeric vector or as a list with elements `from`, `to`, and `by`.
#'
#' If `definitions[[property]]` is a list with `from`/`to`/`by`, this returns
#' the full numeric sequence `seq(from, to, by)`. If it is a vector, it returns
#' that vector coerced to numeric. If the property is `NULL` or missing, returns
#' `NULL`.
#'
#' @param definitions A list holding definition data.
#' @param property    Character scalar; name of the property inside `definitions`.
#'
#' @return A numeric vector (possibly length 0) or `NULL` if not present.
#' @examples
#' defs <- list(planting_dates = list(from = 150, to = 240, by = 15))
#' get_values_by_sequence_or_list(defs, "planting_dates")
#' #> 150 165 180 195 210 225 240
#'
#' defs2 <- list(planting_length = c(60, 75, 90))
#' get_values_by_sequence_or_list(defs2, "planting_length")
#' #> 60 75 90
get_values_by_sequence_or_list <- function(definitions, property) {
  if (is.null(definitions) || is.null(definitions[[property]])) return(NULL)
  obj <- definitions[[property]]
  if (is.list(obj)) {
    from <- as.numeric(obj[["from"]])
    to   <- as.numeric(obj[["to"]])
    by   <- as.numeric(obj[["by"]])
    return(seq(from, to, by))
  }
  as.numeric(obj)
}

#' Build crops definitions (internal helper)
#'
#' Thin wrapper around \code{rpicsa::crops_definitions()} to keep the call site
#' concise and consistent. Not exported.
#'
#' @param data_frame,seasonal_data_frame Data frame (or name) for daily and seasonal data.
#' @param data_names,seasonal_data_names Lists with element names used by the function
#'   (e.g., \code{date}, \code{year}, \code{station}, \code{doy}, \code{rain}, and
#'   \code{start_day}, \code{end_day} for seasonal).
#' @param s_start_month Numeric month (1–12) at which shifted year starts.
#' @param rain_totals,plant_days,plant_lengths Numeric vectors for parameter grids.
#' @param start_check Character scalar understood by \code{rpicsa} (e.g., \code{"yes"} or \code{"both"}).
#' @param return_crops_table Logical; whether to return the full crops table.
#' @param definition_props Logical; whether to include definition properties.
#' @param data_book A data book object passed to \code{rpicsa::crops_definitions()}.
#'
#' @return What \code{rpicsa::crops_definitions()} returns.
#' @keywords internal
.build_crops_call <- function(data_frame, data_names,
                              seasonal_data_frame, seasonal_data_names,
                              s_start_month, rain_totals, plant_days, plant_lengths,
                              start_check, return_crops_table, definition_props,
                              data_book) {
  rpicsa::crops_definitions(
    data_name         = data_frame,
    date_time         = data_names$date,
    year              = data_names$year,
    station           = data_names$station,
    doy               = data_names$doy,
    rain              = data_names$rain,
    s_start_month     = s_start_month,
    rain_totals       = rain_totals,
    plant_days        = plant_days,
    plant_lengths     = plant_lengths,
    start_check       = start_check,
    season_data_name  = seasonal_data_frame,
    start_day         = seasonal_data_names$start_day,
    end_day           = seasonal_data_names$end_day,
    return_crops_table = return_crops_table,
    definition_props   = definition_props,
    data_book          = data_book
  )
}

#' Update crops definitions from stored configuration
#'
#' Reads the \emph{crop success} and (optionally) \emph{season start probabilities}
#' definition blocks and produces crops definition tables via
#' \code{rpicsa::crops_definitions()}.
#'
#' If only one block is present, returns a single table based on that block.
#' If both blocks are present and their parameter grids match (same
#' \code{rain_totals}, \code{plant_days}, \code{plant_lengths}), a single
#' table is returned with \code{definition_props = TRUE}. If both exist but
#' differ, both tables are generated and returned in a list.
#'
#' @param data_frame            A data frame (or name) containing daily data.
#' @param data_names            A list with element names for daily data:
#'   \code{date}, \code{year}, \code{station}, \code{doy}, \code{rain}.
#' @param seasonal_data_frame   A data frame (or name) containing seasonal windows.
#' @param seasonal_data_names   A list with element names for seasonal data:
#'   \code{start_day}, \code{end_day}.
#' @param definitions           A list with (optional) elements
#'   \code{$crops_success} and \code{$season_start_probabilities}. Each may
#'   contain entries such as \code{s_start_month}, \code{start_check},
#'   \code{return_crops_table}, and grid definitions under keys like
#'   \code{water_requirements}, \code{planting_dates}, \code{planting_length},
#'   or \code{specified_day}. Grids may be vectors or lists with
#'   \code{from}/\code{to}/\code{by}.
#' @param data_book             The data book object forwarded to
#'   \code{rpicsa::crops_definitions()}.
#'
#' @details
#' This function converts stored month dates in \code{s_start_month} to a
#' numeric month (\code{1–12}) via \code{as.Date(..., origin = "2020-12-31")}
#' to avoid leap year ambiguity. Logical-like flags (\code{start_check},
#' \code{return_crops_table}, \code{definition_props}) are expected to be
#' provided upstream (e.g., as \code{TRUE}/\code{FALSE} or strings that a
#' project helper such as \code{as_logical()} converts).
#'
#' @return
#' - A single object returned by \code{rpicsa::crops_definitions()} if only one
#'   definition block is active or both blocks match.
#' - A list with elements \code{$crop_success} and
#'   \code{$season_start_probabilities} if both blocks exist and differ.
#'
#' @examples
#' \dontrun{
#' defs <- list(
#'   crops_success = list(
#'     s_start_month =  "2021-01-01",
#'     start_check = TRUE,
#'     return_crops_table = TRUE,
#'     water_requirements = list(from = 100, to = 200, by = 25),
#'     planting_dates     = c(150, 165, 180),
#'     planting_length    = c(60, 75, 90)
#'   )
#' )
#' update_crops_definitions(daily_df, list(date="date", year="year", station="id",
#'                          doy="doy", rain="rain"),
#'                          seasonal_df, list(start_day="s_day", end_day="e_day"),
#'                          defs, data_book)
#' }
#' @export
update_crops_definitions <- function(data_frame, data_names,
                                     seasonal_data_frame, seasonal_data_names,
                                     definitions, data_book) {
  # unpack top-level definitions
  actual_crop_definitions      <- definitions$crops_success
  season_start_probs_definitions <- definitions$season_start_probabilities
  
  if (!is.null(actual_crop_definitions)) return_crops_table <- actual_crop_definitions$return_crops_table
  else return_crops_table <- FALSE
  
  if (!is.null(season_start_probs_definitions)) definition_props <- season_start_probs_definitions$definition_props
  else definition_props <- FALSE
  
  # ---- crop_success (may be NULL) ----
  if (return_crops_table) {
    s_start_month <- as.numeric(format(as.Date(actual_crop_definitions$s_start_month, origin = "2020-12-31"), "%m"))   # picking a non-leap-year
    start_check         <- as_logical(actual_crop_definitions$start_check)
    return_crops_table  <- as_logical(actual_crop_definitions$return_crops_table)
    rain_totals         <- get_values_by_sequence_or_list(actual_crop_definitions, "water_requirements")
    plant_days          <- get_values_by_sequence_or_list(actual_crop_definitions, "planting_dates")
    plant_lengths       <- get_values_by_sequence_or_list(actual_crop_definitions, "planting_length")
  } else {
    s_start_month      <- NULL
    start_check        <- FALSE
    return_crops_table <- FALSE
    rain_totals        <- NULL
    plant_days         <- NULL
    plant_lengths      <- NULL
  }
  
  # ---- season_start_probabilities (may be NULL) ----
  if (definition_props) {
    season_s_start_month <- as.numeric(format(as.Date(season_start_probs_definitions$s_start_month, origin = "2020-12-31"), "%m"))   # picking a non-leap-year
    season_start_check     <- as_logical(season_start_probs_definitions$start_check)
    season_definition_props <- as_logical(season_start_probs_definitions$definition_props)
    season_planting_days   <- get_values_by_sequence_or_list(season_start_probs_definitions, "planting_dates")
    season_rain_totals     <- get_values_by_sequence_or_list(season_start_probs_definitions, "water_requirements")
    season_plant_lengths          <- get_values_by_sequence_or_list(season_start_probs_definitions, "specified_day") # planting_length
    if (is.null(season_plant_lengths)) season_plant_lengths <- get_values_by_sequence_or_list(season_start_probs_definitions, "planting_length")
  } else {
    season_s_start_month    <- NULL
    season_start_check      <- FALSE
    season_definition_props <- FALSE
    season_planting_days    <- NULL
    season_rain_totals      <- NULL
    season_plant_lengths    <- NULL
  }
  
  if (season_start_check == TRUE) season_start_check  <- "both"
  else season_start_check <- "yes"
  if (start_check == TRUE) start_check  <- "both"
  else start_check <- "yes"
  
  # ---- branch by presence of definitions ----
  if (definition_props == FALSE && return_crops_table) {
    # only crop_success
    out <- .build_crops_call(
      data_frame, data_names, seasonal_data_frame, seasonal_data_names,
      s_start_month, rain_totals, plant_days, plant_lengths,
      start_check, return_crops_table, definition_props = FALSE,
      data_book
    )
    return(out)
  }
  
  if ((definition_props == TRUE) && (return_crops_table == FALSE)) {
    # only season_start_probabilities (note: parameter names differ)
    out <- .build_crops_call(
      data_frame, data_names, seasonal_data_frame, seasonal_data_names,
      season_s_start_month, season_rain_totals, season_planting_days, season_plant_lengths,
      season_start_check, return_crops_table = FALSE, definition_props = season_definition_props,
      data_book
    )
    return(out)
  }
  
  # both exist: compare the core grids
  same_grids <- isTRUE(identical(rain_totals, season_rain_totals)) &&
    isTRUE(identical(plant_days,  season_planting_days)) &&
    isTRUE(identical(plant_lengths, season_plant_lengths))
  
  if (same_grids) {
    # unified single output with definition_props = TRUE
    out <- .build_crops_call(
      data_frame, data_names, seasonal_data_frame, seasonal_data_names,
      s_start_month, rain_totals, plant_days, plant_lengths,
      start_check, return_crops_table, definition_props = TRUE,
      data_book
    )
    return(out)
  }
  
  # differ: warn and return both
  warning("Season Start Probabilities definition differs from Crop Success; generating both tables.")
  cs <- .build_crops_call(
    data_frame = data_frame,
    data_names = data_names, 
    seasonal_data_frame = seasonal_data_frame,
    seasonal_data_names = seasonal_data_names,
    s_start_month = s_start_month,
    rain_totals = rain_totals,
    plant_days = plant_days,
    plant_lengths = plant_lengths,
    start_check = start_check,
    return_crops_table = return_crops_table,
    definition_props = FALSE, # season_definition_props
    data_book
  )
  ss <- .build_crops_call(
    data_frame = data_frame,
    data_names = data_names, 
    seasonal_data_frame = seasonal_data_frame,
    seasonal_data_names = seasonal_data_names,
    s_start_month = s_start_month,
    rain_totals = season_rain_totals,
    plant_days = season_planting_days,
    plant_lengths = season_plant_lengths,
    start_check = start_check,
    return_crops_table = FALSE,
    definition_props = season_definition_props,
    data_book
  )
  return(list(crop_success = cs, season_start_probabilities = ss))
}