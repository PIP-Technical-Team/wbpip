#' Gini coefficient
#'
#' Compute the Gini coefficient for microdata.
#'
#' Given a vector of income or consumption values and their respective weights
#' `md_compute_gini()` computes the Gini coefficient for the distribution.
#'
#' @inheritParams compute_pip_stats
#' @param weight numeric: A vector of population weights, optional, a vector
#' of 1s if not specified.
#' @examples
#' md_compute_gini(welfare = 1:2000, weight = rep(1, 2000))
#' @return numeric
#' @export
md_compute_gini <- function(welfare, weight) {

  # Compute weighted welfare
  weighted_welfare     <- welfare * weight
  weighted_welfare_lag <- flag(weighted_welfare,
                               fill = 0)

  # Compute area under the curve using
  # Area of trapezoid = Base * Average height
  v <- (fcumsum(weighted_welfare_lag) + (weighted_welfare / 2)) * weight
  auc <- sum(v) # Area Under the Curve

  # Compute Area Under the Lorenz Curve
  # Normalize auc so it is always between 0 and 0.5
  auc <- (auc / sum(weight)) / sum(weighted_welfare)

  # Compute Gini
  gini <- 1 - (2 * auc)

  return(gini)
}
