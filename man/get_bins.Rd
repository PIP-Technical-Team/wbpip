% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_bins.R
\name{get_bins}
\alias{get_bins}
\title{Bins}
\usage{
get_bins(
  .data,
  welfare,
  weight,
  distribution_type = c("micro", "group", "aggregate", "imputed"),
  nbins = 100,
  output = "simple"
)
}
\arguments{
\item{.data}{Household survey data frame with at least a welfare variable.}

\item{welfare}{numeric: A vector of income or consumption values.}

\item{weight}{numeric: A vector of weights.}

\item{distribution_type}{character: Type of distribution, either micro,
group, aggregate or imputed.}

\item{nbins}{numeric: Number of bins.}

\item{output}{character: It has two varieties. 1) it could be a vector of
variables to retain after calculations (variables available are "welfare",
"weight", "cum_pop", "cum_prop_pop", and "bins"). 2) It could be a one of
two key words, "simple" or "full". \code{output = "simple"} is equivalent to
\code{output = "bins"} (which is the default). \code{output = "full"} if equivalent
to a vector with all the variables available, \code{output = c("welfare", "weight", "cum_pop", "cum_prop_pop","bins")}}
}
\value{
data.frame
}
\description{
Calculate bins. \strong{TO BE DOCUMENTED}.
}
\examples{
data("md_ABC_2000_income")
df <- md_ABC_2000_income

bins <- get_bins(df, welfare, weight)
str(bins)

bins <- get_bins(df, welfare, weight, output = "full")
str(bins)

}
