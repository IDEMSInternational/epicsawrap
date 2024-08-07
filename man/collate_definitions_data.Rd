% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/collate_definitions_data.R
\name{collate_definitions_data}
\alias{collate_definitions_data}
\title{Collate Definitions Data for Climatic Analysis from R-Instat}
\usage{
collate_definitions_data(
  data_by_year = "ghana_by_station_year",
  data_by_year_month = NULL,
  crop_data = "crop_def",
  rain = data_book$get_climatic_column_name(data_name = "ghana", "rain"),
  year = data_book$get_climatic_column_name("ghana", "year"),
  month = data_book$get_climatic_column_name("ghana", "month"),
  summaries = c("annual_rainfall", "annual_temperature", "monthly_temperature",
    "extremes", "crop_success", "start_season"),
  start_rains_column = "start_rain",
  start_rains_status_column = "start_rain_status",
  end_rains_column = "end_rains",
  end_rains_status_column = "end_rain_status",
  end_season_column = "end_season",
  end_season_status_column = "end_season_status",
  seasonal_length_column = "seasonal_length",
  min_tmin_column = "min_tmin",
  mean_tmin_column = "mean_tmin",
  max_tmin_column = "max_tmin",
  min_tmax_column = "min_tmax",
  mean_tmax_column = "mean_tmax",
  max_tmax_column = "max_tmax"
)
}
\arguments{
\item{data_by_year}{The name of the data set that contains data aggregated by year, default is "ghana_by_station_year".}

\item{data_by_year_month}{The name of the data set that contains data aggregated by year and month, default is NULL.}

\item{crop_data}{The name of the crop data set, default is "crop_def".}

\item{rain}{The name of the column containing rainfall data.}

\item{year}{The name of the column containing year data.}

\item{month}{The name of the column containing month data.}

\item{summaries}{The name of the summaries to show. Options are \code{"annual_rainfall"}, \code{"annual_temperature"}, \code{"monthly_temperature"}, \code{"extremes"}, \code{"crop_success"}, \code{"start_season"}.}

\item{start_rains_column}{The name of the start of rains column in the data.}

\item{start_rains_status_column}{The name of the start of rains status column in the data.}

\item{end_rains_column}{The name of the end of rains column in the data.}

\item{end_rains_status_column}{The name of the end of rains status column in the data.}

\item{end_season_column}{The name of the end of season column in the data.}

\item{end_season_status_column}{The name of the end of seasons status column in the data.}

\item{seasonal_length_column}{The name of the seasonal length column in the data.}

\item{min_tmin_column}{The name of the minimum of minimum temperature column in the data.}

\item{mean_tmin_column}{The name of the mean of minimum temperature column in the data.}

\item{max_tmin_column}{The name of the maximum of minimum temperature column in the data.}

\item{min_tmax_column}{The name of the minimum of maximum temperature column in the data.}

\item{mean_tmax_column}{The name of the mean of maximum temperature column in the data.}

\item{max_tmax_column}{The name of the maximum of maximum temperature column in the data.}
}
\value{
A list that contains the aggregated data definitions.
}
\description{
This function aggregates various climatic data definitions, including annual summaries,
temperature summaries, crop data, and probabilities of season starts. It is designed to work
within a specific context that involves climatic data processing and analysis, particularly
focusing on data related to Ghana's climate. The function uses multiple sources of data
and calculations to generate a comprehensive list-formatted summary.
}
\examples{
#data_book <- list(get_climatic_column_name = function(data_name, col_name) { return(col_name) },
#                  get_calculations = function(data_name) { list() },
#                  get_data_frame_metadata = function(data_name) { list() })
#collate_definitions_data(data_book = data_book)

}
