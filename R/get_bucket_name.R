#' Get Bucket Name
#'
#' @param country A character vector specifying the country or countries from which to update the data. Options are `"mz"`, `"zm"`, `"zm_test"`, `"ml_test"`, `"ke_test"`.
#'
#' @return Returns the name of the bucket for the Malawi or Zambia data.
#'
#' @examples #get_bucket_name("mw")
get_bucket_name <- function(country = c("mw", "zm", "zm_test", "ml_test", "ke_test", "internal_tests", "zm_workshops", "mw_workshops")) {
  if (length(country) > 1) stop("'country' must be length 1.")
  country <- match.arg(country)
  if (country == "mw") return("malawi_data")
  else if (country == "zm") return("zambia_data")
  else if (country == "zm_test") return("zambia_test_data")
  else if (country == "ml_test") return("malawi_test_data")
  else if (country == "ke_test") return("kenya_test_data")
  else if (country == "internal_tests") return("climsoft_data")
  else if (country == "zm_workshops") return("zm_workshops")
  else if (country == "mw_workshops") return("mw_workshops")
}