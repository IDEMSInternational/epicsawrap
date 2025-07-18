% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reformat_functions.R
\name{reformat_annual_summaries}
\alias{reformat_annual_summaries}
\title{Reformat Annual Climate Summaries}
\usage{
reformat_annual_summaries(
  data,
  station_col = NULL,
  year_col = NULL,
  start_rains_doy_col = NULL,
  start_rains_date_col = NULL,
  end_rains_doy_col = NULL,
  end_rains_date_col = NULL,
  end_season_doy_col = NULL,
  end_season_date_col = NULL,
  seasonal_rain_col = NULL,
  n_seasonal_rain_col = NULL,
  season_length_col = NULL,
  annual_rain_col = NULL,
  extreme_rain_days_col = NULL,
  n_rain_col = NULL
)
}
\arguments{
\item{data}{A data frame containing annual summaries.}

\item{station_col}{Name of the column containing station information.}

\item{year_col}{Name of the column containing year information.}

\item{start_rains_doy_col}{Column for start of rains (day-of-year).}

\item{start_rains_date_col}{Column for start of rains (date).}

\item{end_rains_doy_col}{Column for end of rains (day-of-year).}

\item{end_rains_date_col}{Column for end of rains (date).}

\item{end_season_doy_col}{Column for end of season (day-of-year).}

\item{end_season_date_col}{Column for end of season (date).}

\item{seasonal_rain_col}{Column for total seasonal rainfall.}

\item{n_seasonal_rain_col}{Column for number of seasonal rain days.}

\item{season_length_col}{Column for season length (days).}

\item{annual_rain_col}{Column for total annual rainfall.}

\item{extreme_rain_days_col}{Column for number of extreme rain days.}

\item{n_rain_col}{Column for total number of rain days.}
}
\value{
A reformatted data frame with standardized column names and cleaned data types.
}
\description{
This function standardizes column names and converts data types for annual climate summaries.
It prepares the dataset for further processing or export, ensuring consistency in variable naming
and data formats (e.g., converting dates, rain counts, and DOY values).
}
