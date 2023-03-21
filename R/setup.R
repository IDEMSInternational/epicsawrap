#' Setup directories
#'
#' @param dir `character(1)` The path to set as the working directory
#' @param countries `character(1)` The set of countries to create directories
#'   for.
#'
#' @return
#' @export
#'
#' @examples
setup <- function(dir, countries = c("mw", "zm")) {
  if (!dir.exists(dir)) dir.create(dir)
  setwd(dir)
  for (i in seq_along(countries)) {
    cnty_path <- countries[i]
    data_path <- paste0(cnty_path, "/", "data")
    if (!dir.exists(cnty_path)) {
      dir.create(cnty_path)
    }
    if (!dir.exists(data_path)) {
      dir.create(data_path)
    }
  }
}