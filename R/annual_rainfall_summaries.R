#' Annual Rainfall Summaries
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
#' # annual_rainfall_summaries(country = "zm", station_id = "01122")
annual_rainfall_summaries <- function(country,
                                      station_id, 
                                      summaries = c("seasonal_rainfall",
                                                    "seasonal_raindays",
                                                    "start_rains",
                                                    "end_season",
                                                    "length_season")) {
  summaries <- match.arg(summaries)
  daily <- epicsadata::run_daily_data(country = country, 
                                      station_id = station_id)
  # another call - epicsadata::get_definitions_data()
  annual <- rpicsa::annual_rain(daily, date_time = "date", rain = "rain",
                                year = "year", station = "station",
                                na_rm = FALSE)
  list_return <- NULL
  # anything defined in the json to go in here
  # and to be returned in that format (e.g. dataframe, list of lists, etc)
  list_return[[1]] <- c("country" = country,
                        "station_id" = station_id,
                        "summaries" = summaries)
  list_return[[2]] <- annual
  return(list_return) # return a list with in it the metadata and the data itself
}

# Another function;
# one which will save this to the bucket
# another which will check if this is in the bucket, and if so will call from the bucket
## if not, it will run this function above (i.e., do teh calculation and save it to the bucket)

# When requesting summaries - always get them from the bucket
# don't bother having a saved local version
# no point saving files to then do read.rds
# always call from google cloud

# for summaries - no need to save it
# (might have to to write it in)
# but can we just write it straight to the cloud?
# do we have to make it as an RDS to save it into the cloud? Might have to.

# epicsadata:::update_data
# that's the only place we use googledrive to get thedata. 

# seasonal forecast to be like PDF files
# so python fn needs to send that PDF to the app, so, the file should be downloaded
# so download that in the data package.
# download and save to somewhere on the disc
# add parameter into function, or have a separate function, to the update_data
# want to return the output from google cloud storage
# because sometimes we will want the save_to and to download it. 
# sometimes we want 