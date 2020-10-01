#' Clean welfare and weight inputs for microdata.
#'
#' Remove any negative or missing values, and make sure that welfare is sorted
#' in increasing order.
#'
#' @param welfare numeric: A vector of income or consumption values.
#' @param weight numeric: A vector of weights.
#' @return list
#' @noRd
md_clean_inputs <- function(welfare, weight){

  # Remove missing values
  if (anyNA(welfare) | anyNA(weight)) {
    if (anyNA(welfare)) {
      if (sum(is.na(welfare)) == 1) {
        msg <- sprintf('Info: Found 1 missing value in `welfare`. This observation was removed.')
      } else {
        msg <- sprintf('Info: Found %s missing values in `welfare`. These observations were removed.',
                       sum(is.na(welfare)))
      }
      rlang::inform(msg)
    }
    if (anyNA(weight)) {
      if (sum(is.na(weight)) == 1) {
        msg <- sprintf('Info: Found 1 missing value in `weight`. This observation was removed.')
      } else {
        msg <- sprintf('Info: Found %s missing values in `weight`. These observations were removed.',
                       sum(is.na(weight)))
      }
      rlang::inform(msg)
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
      rlang::inform(msg)
    }
    if (any(weight < 0)) {
      if (sum(weight < 0) == 1) {
        msg <- sprintf('Info: Found 1 negative value in `weight`. This observation was removed.')
      } else {
        msg <- sprintf('Info: Found %s negative values in `weight`. These observations were removed.',
                       sum(weight < 0))
      }
      rlang::inform(msg)
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

#' Check that welfare and weight inputs for microdata are valid.
#'
#' @inheritParams md_clean_inputs
#' @return logical
#' @noRd
md_check_inputs <- function(welfare, weight){
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
