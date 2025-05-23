% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_summaries_from_definition.R
\name{update_rainfall_summaries_from_definition}
\alias{update_rainfall_summaries_from_definition}
\title{Update Rainfall Summaries from Definitions}
\usage{
update_rainfall_summaries_from_definition(
  country = "zm_workshops",
  station_id = NULL,
  definition_id = NULL,
  daily_data,
  summaries = c("annual_rain", "start_rains", "end_rains", "end_season", "seasonal_rain",
    "seasonal_length")
)
}
\arguments{
\item{country}{Character. The name of the country for which definitions and observations are retrieved. Defaults to \code{"zm_workshops"}.}

\item{station_id}{Character. The station ID(s) for which data is retrieved. Can be \code{NULL} if \code{definition_id} is specified. Defaults to \code{NULL}.}

\item{definition_id}{Character. The ID of the definitions to use for generating summaries. Only used if \code{station_id} is \code{NULL}. Defaults to \code{NULL}.}

\item{daily_data}{Data frame. The daily rainfall data used to generate summaries.}

\item{summaries}{Character vector. The names of the summaries to produce. Options include:
\itemize{
\item \code{"annual_rain"}: Calculates total annual rainfall.
\item \code{"start_rains"}: Identifies the start of the rainy season.
\item \code{"end_rains"}: Identifies the end of the rainy season.
\item \code{"end_season"}: Identifies the end of the season.
\item \code{"seasonal_rain"}: Calculates total seasonal rainfall.
\item \code{"seasonal_length"}: Calculates the length of the rainy season.
Defaults to all available summaries.
}}
}
\value{
A data frame containing the requested rainfall summaries for the specified station(s) or definitions.
}
\description{
This function generates rainfall summary data based on the most recent definitions and observed rainfall data
for a specified country. Summaries include metrics such as start and end of rains, seasonal length, seasonal rainfall,
and annual rainfall, depending on the definitions and specified summary types.
}
\details{
\itemize{
\item If \code{station_id} is provided, the function retrieves the corresponding definitions data for the station.
\item If \code{station_id} is \code{NULL}, the function fetches definitions data directly using the \code{definition_id}.
\item The function only calculates the summaries specified in the \code{summaries} argument.
\item Summary calculations depend on definitions being available for the requested metrics.
}
}
