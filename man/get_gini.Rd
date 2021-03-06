% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_gini.R
\name{get_gini}
\alias{get_gini}
\title{Gini coefficient}
\usage{
get_gini(
  .data,
  welfare,
  weight,
  distribution_type = c("micro", "group", "aggregate", "imputed")
)
}
\arguments{
\item{.data}{Household survey data frame with at least a welfare variable.}

\item{welfare}{numeric: A vector of income or consumption values.}

\item{weight}{numeric: A vector of weights.}

\item{distribution_type}{character: Type of distribution, either micro,
group, aggregate or imputed.}
}
\value{
data.table
}
\description{
Compute the Gini coefficient.
}
