#' Annual Rainfall Summaries
#' 
#' @description A table containing all the annual rainfall summaries for PICSA
#' e.g. start of rain, total rainfall, number of rain days, end of season.
#' One row per year/station and one column per summary.
#'
#' @param country `character(1)` The country code of the data.
#' @param station_id `character` The id's of the stations to analyse. Either a
#'   single value or a vector.
#' @param summaries `character` The names of the summaries to produce.
#'
#' @return A data frame with yearly summaries.
#' @export
#'
#' @examples
#' annual_rainfall_summaries(country = "zm", station_id = "01122", summaries = "annual_rain")
annual_rainfall_summaries <- function(country,
                                      station_id, 
                                      # for now, just one summary: annual_rain
                                      summaries = c("annual_rain", # total rain days and annual rainfall
                                                    "start_rains",
                                                    "end_rains")
                                      # TODO: add in 
                                      # ("seasonal_rainfall",
                                      # "seasonal_raindays",
                                      ) {
  # cheaper to not save this and to just call it?
  daily <- epicsadata::get_daily_data(country = country, station_id = station_id)
  
  definitions <- epicsadata::get_definitions_data(def_data = def_data, summaries = summaries)
  definitions <- purrr::map(.x = summaries, .f = ~ definitions[[.x]])
  names(definitions) <- summaries
  # epicsadata::get_definitions_data()
  summary_data <- expand.grid(year = unique(daily$year), station = unique(daily$station))
  if ("annual_rain" %in% summaries){
    annual_rain <- rpicsa::annual_rain(daily, date_time = "date", rain = "rain", year = "year", station = "station",
                                       # how does it know the names of these from the daily_data? Is it always the same?
                                       total_rain = as.logical(definitions$annual_rain$total_rain),
                                       n_rain = as.logical(definitions$annual_rain$n_rain),
                                       na_rm = as.logical(definitions$annual_rain$na_rm),
                                       na_prop = definitions$annual_rain$na_prop,
                                       na_n = definitions$annual_rain$na_n,
                                       na_consec = definitions$annual_rain$na_consec,
                                       na_n_non = definitions$annual_rain$na_n_non)
    summary_data <- dplyr::full_join(summary_data, annual_rain)
  }
  if ("start_rains" %in% summaries){
    start_rains <- rpicsa::start_rains(daily, date_time = "date", station = "station", year = "year", rain = "rain", doy = "doy",
                                       threshold  = definitions$start_rains$threshold,
                                       start_day  = definitions$start_rains$start_day,
                                       end_day = definitions$start_rains$end_day,
                                       output = definitions$start_rains$output,
                                       total_rainfall = as.logical(definitions$start_rains$total_rainfall),
                                       over_days = definitions$start_rains$over_days,
                                       amount_rain = definitions$start_rains$amount_rain,
                                       proportion = as.logical(definitions$start_rains$proportion),
                                       number_rain_days = as.logical(definitions$start_rains$number_rain_days),
                                       dry_spell = as.logical(definitions$start_rains$dry_spell),
                                       spell_interval = definitions$start_rains$spell_interval,
                                       spell_max_dry_days = definitions$start_rains$spell_max_dry_days,
                                       dry_period = as.logical(definitions$start_rains$dry_period),
                                       period_interval = definitions$start_rains$period_interval,
                                       max_rain = definitions$start_rains$max_rain,
                                       period_max_dry_days = definitions$start_rains$period_max_dry_days)
    summary_data <- dplyr::full_join(summary_data, start_rains)
  }
  if ("end_rains" %in% summaries){
    end_rains <- rpicsa::end_rains(daily, date_time = "date", station = "station", year = "year", rain = "rain", doy = "doy",
                               start_day  = definitions$end_rains$start_day,
                               end_day = definitions$end_rains$end_day,
                               output = definitions$end_rains$output,
                             interval_length = definitions$end_rains$interval_length,
                             min_rainfall = definitions$end_rains$min_rainfall)
    summary_data <- dplyr::full_join(summary_data, end_rains)
  }
  
  
  list_return <- NULL
  # anything defined in the json to go in here
  # and to be returned in that format (e.g. dataframe, list of lists, etc)
  list_return[[1]] <- c(definitions)
  list_return[[2]] <- summary_data
  return(list_return) # return a list with in it the metadata and the data itself
}
