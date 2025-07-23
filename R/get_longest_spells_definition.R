#' Get Longest Spell Definitions
#'
#' Extract the numeric lower and upper bounds from a spell‑length calculation
#' expression stored in a year’s definitions list.
#'
#' @param definitions_year A list (or similar structure) containing spell definitions
#'   for a specific year. One element should hold an object with a
#'   `spell_length$spell_day` entry whose second element is a string like
#'   `"(rain >= X) & rain <= Y"`.
#' @param spell_column A string or integer specifying which element of
#'   `definitions_year` contains the spell information. If `NULL`, no spell is
#'   extracted and both bounds return as `NA`.
#' @param list_name Character. The name to use for the returned sub‑list. Defaults
#'   to `"longest_spell"`.
#'
#' @return A named list of length 1. The single element is named by `list_name`
#'   and contains a list with two numeric components:
#'   - `spell_from`: the lower bound \(X\) extracted from the expression, or `NA`.
#'   - `spell_to`: the upper bound \(Y\) extracted from the expression, or `NA`.
#'
#' @details
#' This function looks up `definitions_year[[spell_column]]`, then expects to find
#' a string at
#' `spell$spell_length$spell_day[[2]]` matching the pattern
#' `"...>= X ...<= Y"`. It uses regular expressions to pull out the numbers
#' `X` (after `>=`) and `Y` (after `<=`). If the column is missing or `NULL`,
#' both `spell_from` and `spell_to` are set to `NA`.
#'
#' @examples
#' definitions_year <- list(
#'   my_spell = list(
#'     spell_length = list(
#'       spell_day = list(NULL, "(rain >= 0.85) & rain <= 2.3")
#'     )
#'   )
#' )
#' get_longest_spell_definitions(definitions_year, "my_spell")
#' # $longest_spell
#' # $longest_spell$spell_from
#' # [1] 0.85
#' #
#' # $longest_spell$spell_to
#' # [1] 2.3
#'
#' @export
get_longest_spell_definitions <- function(definitions_year, spell_column, list_name = "longest_spell"){
  # Create an empty list
  data_list <- list()
  
  if (!is.null(spell_column)) spell <- definitions_year[[spell_column]]
  else spell <- NULL
  
  if (!is.null(spell)) {
    spell_calculation <- spell$spell_length$spell_day[[2]]

    data_list[[list_name]][["spell_from"]] <- as.numeric(sub(".*>=\\s*([0-9]+\\.?[0-9]*).*", "\\1", spell_calculation))
    data_list[[list_name]][["spell_to"]]   <- as.numeric(sub(".*<=\\s*([0-9]+\\.?[0-9]*).*", "\\1", spell_calculation))
  } else {
    data_list[[list_name]][["spell_from"]] <- NA
    data_list[[list_name]][["spell_to"]]   <- NA
  }
  
  return(data_list)
}
