% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prod_gd_compute_pip_stats_lb.R
\name{prod_gd_estimate_lb}
\alias{prod_gd_estimate_lb}
\title{Estimates poverty and inequality stats from beta Lorenz fit
Version used in production}
\usage{
prod_gd_estimate_lb(mean, povline, p0, A, B, C)
}
\arguments{
\item{mean}{numeric: Welfare mean.}

\item{povline}{numeric: Poverty line.}

\item{p0}{numeric: \strong{TO BE DOCUMENTED}.}

\item{A}{numeric: Lorenz curve coefficient. Output of
\code{regres()$coef[1]}.}

\item{B}{numeric: Lorenz curve coefficient. Output of
\code{regres()$coef[2]}.}

\item{C}{numeric: Lorenz curve coefficient. Output of
\code{regres()$coef[3]}.}
}
\value{
list
}
\description{
Estimates poverty and inequality stats from beta Lorenz fit
Version used in production
}
\keyword{internal}
