#' @importFrom dplyr lag
NULL

#' Gini coefficient
#'
#' Compute the Gini coefficient for microdata.
#'
#' Given a vector of income or consumption values and their respective weights
#' `md_compute_gini()` computes the Gini coefficient for the distribution.
#'
#' Note that negative values will be removed from the calculation.
#'
#' @param welfare numeric: A vector of income or consumption values.
#' @param weight numeric: A vector of weights, optional.
#' @inheritParams md_clean_data
#' @return numeric
#'
#' @examples
#' # Simple example
#' md_compute_gini(welfare = 1:10)
#'
#' # Include weights
#' md_compute_gini(welfare = 1:10, weight = 1:10)
#'
#' # Microdata example
#' data("md_ABC_2000_income")
#' md_compute_gini(md_ABC_2000_income$welfare, md_ABC_2000_income$weight)
#'
#' @export
md_compute_gini <- function(welfare,
                            weight = NULL,
                            type = "microdata"){

  # Set all weights to 1 if none are supplied
  if (is.null(weight)) weight <- rep(1, length(welfare))

  # Make sure data is sorted
  ordy    <- order(welfare)   # order of welfare
  welfare <- welfare[ordy]    #order weight
  weight  <- weight[ordy]     # order welfare

  # Calculate Gini
  weighted_welfare <- welfare * weight
  weighted_welfare_lag <- dplyr::lag(weighted_welfare, default = 0)
  # Compute area under the curve using
  # Area of trapezoid = Base * Average height
  v <- (cumsum(weighted_welfare_lag) + weighted_welfare / 2) * weight
  auc <- sum(v) # Area Under the Curve
  # Compute Area Under the Lorenz Curve
  # Normalize auc so it is always between 0 and 0.5
  auc <- auc / sum(weight) / sum(weighted_welfare)
  # Compute gini
  gini <- 1 -  2 * auc

  return(gini)
}
