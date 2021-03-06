% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/md_compute_gini.R
\name{md_compute_gini}
\alias{md_compute_gini}
\title{Gini coefficient}
\usage{
md_compute_gini(welfare, weight)
}
\arguments{
\item{welfare}{numeric: A vector of income or consumption values.}

\item{weight}{numeric: A vector of weights.}
}
\value{
numeric
}
\description{
Compute the Gini coefficient for microdata.
}
\details{
Given a vector of income or consumption values and their respective weights
\code{md_compute_gini()} computes the Gini coefficient for the distribution.
}
\examples{
wbpip:::md_compute_gini(welfare = 1:2000, weight = rep(1, 2000))

}
\keyword{internal}
