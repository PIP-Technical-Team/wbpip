#' Gini coefficient
#'
#' Compute the Gini coefficient for microdata.
#'
#' Given a vector of income or consumption values and their respective weights
#' `md_compute_gini()` computes the Gini coefficient for the distribution.
#'
#' @param welfare numeric: A vector of income or consumption values.
#' @param weight numeric: A vector of weights.
#'
#' @examples
#' wbpip:::md_compute_gini(welfare = 1:2000, weight = rep(1, 2000))
#'
#' @return numeric
#' @keywords internal
md_compute_gini <- function(welfare, weight) {

  # Compute weighted welfare
  weighted_welfare <- welfare * weight
  weighted_welfare_lag <- collapse::flag(weighted_welfare, fill = 0)

  # Compute area under the curve using
  # Area of trapezoid = Base * Average height
  v <- (cumsum(weighted_welfare_lag) + (weighted_welfare / 2) ) * weight
  auc <- collapse::fsum(v) # Area Under the Curve

  # Compute Area Under the Lorenz Curve
  # Normalize auc so it is always between 0 and 0.5
  auc <- (auc / collapse::fsum(weight)) / collapse::fsum(weighted_welfare)

  # Compute Gini
  gini <- 1 -  (2 * auc)

  return(gini)
}

