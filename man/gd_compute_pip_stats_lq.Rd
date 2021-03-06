% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gd_compute_pip_stats_lq.R
\name{gd_compute_pip_stats_lq}
\alias{gd_compute_pip_stats_lq}
\title{Computes poverty statistics (Lorenz quadratic)}
\usage{
gd_compute_pip_stats_lq(
  welfare,
  povline,
  population,
  requested_mean,
  popshare = NULL,
  default_ppp = NULL,
  ppp = NULL,
  p0 = 0.5
)
}
\arguments{
\item{welfare}{numeric: Cumulative proportion of welfare held by that
proportion of the population (Lorenz Curve).}

\item{povline}{numeric: Poverty line.}

\item{population}{numeric: Cumulative proportion of population.}

\item{requested_mean}{numeric: Welfare mean.}

\item{popshare}{numeric: Share of population living below the poverty line.
Optional.}

\item{default_ppp}{numeric: Default purchasing power parity.}

\item{ppp}{numeric: PPP request by user.}

\item{p0}{numeric: \strong{TO BE DOCUMENTED}.}
}
\value{
list
}
\description{
Compute poverty statistics for grouped data using the quadratic functional
form of the Lorenz qurve.
}
\examples{
# Set initial parameters
L <- c(0.00208, 0.01013, 0.03122, 0.07083, 0.12808, 0.23498, 0.34887,
  0.51994, 0.6427, 0.79201, 0.86966, 0.91277, 1)
P <- c(0.0092, 0.0339, 0.085, 0.164, 0.2609, 0.4133, 0.5497, 0.7196,
  0.8196, 0.9174, 0.957, 0.9751, 1)
mu  <- 109.9 # mean
z   <- 89    # poverty line

res <- wbpip:::gd_compute_pip_stats_lq(
  welfare = L,
  population = P,
  requested_mean = mu,
  povline = z)
res$headcount

res2 <- wbpip:::gd_compute_pip_stats_lq(
  welfare = L,
  population = P,
  requested_mean = mu,
  popshare = res$headcount)
res2$povline

}
\keyword{internal}
