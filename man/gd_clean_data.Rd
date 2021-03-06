% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gd_clean_data.R
\name{gd_clean_data}
\alias{gd_clean_data}
\title{Clean data (grouped)}
\usage{
gd_clean_data(dt, welfare, population, gd_type, quiet = FALSE)
}
\arguments{
\item{dt}{data.frame: A table with survey data.}

\item{welfare}{character: Name of welfare column.}

\item{population}{character: Name of population column.}

\item{gd_type}{numeric: Type of data. See details.}

\item{quiet}{logical: If TRUE output messages are suppressed.}
}
\value{
data.table
}
\description{
Clean grouped data to be used in PIP methods.
}
\details{
If \code{gd_type = 1} \code{population} must be the cumulative proportion of
population and \code{welfare} must be the cumulative proportion of income held by
that proportion of the population (Lorenz curve). If \code{gd_type = 2},
\code{population} must be the proportion of population and \code{welfare} must be the
proportion of income. If \code{gd_type = 5}, then \code{population} must be the
Percentage of the population in a given interval of incomes, whereas
\code{welfare} must be the mean income of that interval.
}
\keyword{internal}
