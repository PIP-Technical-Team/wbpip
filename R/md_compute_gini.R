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
#' Note that negative values will be removed from the calculation.
#'
#' @param welfare numeric: A vector of income or consumption values.
#' @param weight numeric: A vector of weights, optional.
#'
#' @return numeric
#'
#' @examples
#' # Simple example
#' md_compute_gini(welfare = 1:10)
#'
#' # Include weights
#' md_compute_gini(welfare = 1:10, weight = 1:10)
#'
#' @export
md_compute_gini <- function(welfare, weight = NULL){

  # Set all weights to 1 if none are supplied
  if (is.null(weight)) weight <- rep(1, length(welfare))

  # Check that inputs are valid
  check_inputs_md_compute_gini(welfare = welfare, weight = weight)

  # Clean data if necessary
  res <- clean_inputs_md_compute_gini(welfare = welfare, weight = weight)
  welfare <- res$welfare; weight <- res$weight

  # Calculate Gini
  weighted_welfare <- welfare * weight
  weighted_welfare_lag <- dplyr::lag(weighted_welfare, default = 0)
  v <- (cumsum(weighted_welfare_lag) + weighted_welfare / 2) * weight
  auc <- sum(v) # Area below Lorenz curve
  gini <- 1 - auc / sum(weight) / sum(weighted_welfare) * 2
  return(gini)

}

#' Clean data inputs for `md_compute_gini`.
#'
#' Remove any negative or missing values, and make sure that welfare is sorted
#' in increasing order.
#'
#' @inheritParams md_compute_gini
#' @return list
#' @noRd
clean_inputs_md_compute_gini <- function(welfare, weight){

  # Remove missing values
  if (anyNA(welfare) | anyNA(weight)) {
    if (anyNA(welfare)) {
      if (sum(is.na(welfare)) == 1) {
        msg <- sprintf('Info: Found 1 missing value in `welfare`. This observation was removed.')
      } else {
        msg <- sprintf('Info: Found %s missing values in `welfare`. These observations were removed.',
                       sum(is.na(welfare)))
      }
      message(msg)
    }
    if (anyNA(weight)) {
      if (sum(is.na(weight)) == 1) {
        msg <- sprintf('Info: Found 1 missing value in `weight`. This observation was removed.')
      } else {
        msg <- sprintf('Info: Found %s missing values in `weight`. These observations were removed.',
                       sum(is.na(weight)))
      }
      message(msg)
    }
    weight_no_na <- weight[!is.na(welfare)]
    weight_no_na <- weight_no_na[!is.na(weight_no_na)]
    welfare_no_na <- welfare[!is.na(weight)]
    welfare_no_na <- welfare_no_na[!is.na(welfare_no_na)]
    weight <- weight_no_na
    welfare <- welfare_no_na
  }

  # Remove negative values
  if (any(welfare < 0) | any(weight < 0)) {
    if (any(welfare < 0)) {
      if (sum(welfare < 0) == 1) {
        msg <- sprintf('Info: Found 1 negative value in `welfare`. This observation was removed.')
      } else {
        msg <- sprintf('Info: Found %s negative values in `welfare`. These observations were removed.',
                       sum(welfare < 0))
      }
      message(msg)
    }
    if (any(weight < 0)) {
      if (sum(weight < 0) == 1) {
        msg <- sprintf('Info: Found 1 negative value in `weight`. This observation was removed.')
      } else {
        msg <- sprintf('Info: Found %s negative values in `weight`. These observations were removed.',
                       sum(weight < 0))
      }
      message(msg)
    }
    weight_no_negative <- weight[!welfare < 0]
    weight_no_negative <- weight_no_negative[!weight_no_negative < 0]
    welfare_no_negative <- welfare[!weight < 0]
    welfare_no_negative <- welfare_no_negative[!welfare_no_negative < 0]
    weight <- weight_no_negative
    welfare <- welfare_no_negative
  }

  # Order welfare and weight if welfare is unsorted
  if (is.unsorted(welfare)) {
    welfare_ordered <- order(welfare)
    weight <- weight[welfare_ordered]
    welfare <- sort(welfare)
  }

  out <- list(welfare = welfare, weight = weight)
  return(out)

}

#' Check that welfare and weight inputs are valid.
#'
#' @inheritParams md_compute_gini
#' @return logical
#' @noRd
check_inputs_md_compute_gini <- function(welfare, weight){
  if (!is.numeric(weight))
    rlang::abort(c('`weight` must be a numeric or integer vector:',
                   x = sprintf('You\'ve supplied an object of class %s.', class(weight))))
  if (!is.numeric(welfare))
    rlang::abort(c('`welfare` must be a numeric or integer vector:',
                   x = sprintf('You\'ve supplied an object of class %s.', class(welfare))))
  if (length(weight) != length(welfare))
    rlang::abort(c('`welfare` and `weight` must have compatible lengths:',
                   x = sprintf('`welfare` has length %s.', length(welfare)),
                   x = sprintf('`weight` has length %s.', length(weight))))
}
