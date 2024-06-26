% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/climsoft_connection_functions.R
\name{set_climsoft_conn}
\alias{set_climsoft_conn}
\title{Set Climsoft Connection}
\usage{
set_climsoft_conn(dbname, user, password, host, port)
}
\arguments{
\item{dbname}{Name of the database.}

\item{user}{Username for database access.}

\item{password}{Password for database access.}

\item{host}{Host where the database server is located.}

\item{port}{Port number on which the database server is running.}
}
\value{
Invisible. The function does not return anything but stores the connection in a designated package environment.
}
\description{
Establishes a connection to a Climsoft database and stores it in a package environment for later use.
}
\examples{
#set_climsoft_conn("climsoft_db", "user", "password", "localhost", "3306")

}
