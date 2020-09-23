#' @importFrom dplyr lag
#' @importFrom rlang abort
NULL

#' Gini coefficient
#'
#' Compute the Gini coefficient for microdata.
#'
#' Given a vector of income or consumption values and their respective weights
#' `md_compute_gini()` computes the Gini coefficient for the distribution.
#'
#' Note that the input parameters cannot contain missing or negative values and
#' that `measure` must be sorted in increasing order.
#'
#' @param measure numeric: A vector of income or consumption values.
#' @param weight numeric: A vector of weights.
#'
#' @return numeric
#'
#' @examples
#' # Simple example
#' md_compute_gini(measure = 1:100, weight = rep(1, 100))
#'
#' @export
md_compute_gini <- function(measure, weight){

  # CHECK that inputs are valid
  check_inputs_md_compute_gini(measure, weight)

  # Calculate Gini
  delta_measure <- measure * weight
  delta_measure_lag <- dplyr::lag(delta_measure, default = 0)
  v <- (cumsum(delta_measure_lag) + delta_measure / 2) * weight
  auc <- sum(v) # Area below Lorenz curve
  gini <- 1 - auc / sum(weight) / sum(delta_measure) * 2
  return(gini)
}

#' Check that measure and weight inputs are valid.
#'
#' @inheritParams md_compute_gini
#' @return logical
#' @noRd
check_inputs_md_compute_gini <- function(measure, weight){
  # Validation checks
  if (anyNA(weight)) rlang::abort('`weight` cannot contain missing values.')
  if (anyNA(measure)) rlang::abort('`measure` cannot contain missing values.')
  if (any(weight < 0)) rlang::abort('`weight` cannot contain negative values.')
  if (any(measure < 0)) rlang::abort('`measure` cannot contain negative values.')
  if (!is.numeric(weight))
    rlang::abort(c('`weight` must be a numeric or integer vector:',
                   x = sprintf('You\'ve supplied an object of class %s.', class(weight))))
  if (!is.numeric(measure))
    rlang::abort(c('`measure` must be a numeric or integer vector:',
                   x = sprintf('You\'ve supplied an object of class %s.', class(measure))))
  if (length(weight) != length(measure))
    rlang::abort(c('`measure` and `weight` must have compatible lengths:',
                   x = sprintf('`measure` has length %s.', length(measure)),
                   x = sprintf('`weight` has length %s.', length(weight))))
  if (is.unsorted(measure)) rlang::abort('`measure` must be sorted in increasing order.')
}
