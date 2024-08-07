% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_annual_summaries_definitions.R
\name{build_annual_summaries_definitions}
\alias{build_annual_summaries_definitions}
\title{Get annual summaries definitions}
\usage{
build_annual_summaries_definitions(
  data_name,
  data_by_year,
  rain_name = data_book$get_climatic_column_name(data_name = data_name, col_name =
    "rain"),
  start_rains_column,
  start_rains_status_column,
  end_rains_column,
  end_rains_status_column,
  end_season_column,
  end_season_status_column,
  seasonal_length_column
)
}
\arguments{
\item{data_name}{The name of the data.}

\item{data_by_year}{A list containing definitions for start of rains, end of rains, end of season, and seasonal length.}

\item{rain_name}{The name of the rainfall column in the data.}

\item{start_rains_column}{The name of the start of rains column in the data}

\item{start_rains_status_column}{The name of the start of rains status column in the data}

\item{end_rains_column}{The name of the end of rains column in the data}

\item{end_rains_status_column}{The name of the end of rains status column in the data.}

\item{end_season_column}{The name of the end of season column in the data.}

\item{end_season_status_column}{The name of the end of seasons status column in the data.}

\item{seasonal_length_column}{The name of the seasonal length column in the data}
}
\value{
A list of annual summaries definitions.
}
\description{
Retrieves annual summaries definitions including start of rains, end of rains, end of season, seasonal length,
and annual rainfall summaries.
}
\examples{
# Example usage:
#get_annual_summaries_definitions("data_name", data_by_year, data)
}
