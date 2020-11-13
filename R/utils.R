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

    # Number of observations with missing values
    n_obs <- sum(is.na(welfare) | is.na(weight))

    # Info message
    if (n_obs == 1) {
      msg_main <- 'Info: Found 1 observation with missing values. This observation was removed.'
      if (sum(is.na(weight)) == 1) {
        msg_weight <- '1 missing value in `weight`.'
      } else {
        msg_weight <- sprintf('%s missing values in `weight`.', sum(is.na(weight)))
      }
      if (sum(is.na(welfare)) == 1) {
        msg_welfare <- '1 missing value in `welfare`.'
      } else {
        msg_welfare <- sprintf('%s missing values in `welfare`.', sum(is.na(welfare)))
      }
      msg <- c(msg_main, i = msg_welfare, i = msg_weight)
    } else {
      msg_main <- 'Info: Found %s observations with missing values. These observations were removed.'
      msg_main <- sprintf(msg_main, n_obs)
      if (sum(is.na(weight)) == 1) {
        msg_weight <- '1 missing value in `weight`.'
      } else {
        msg_weight <- sprintf('%s missing values in `weight`.', sum(is.na(weight)))
      }
      if (sum(is.na(welfare)) == 1) {
        msg_welfare <- '1 missing value in `welfare`.'
      } else {
        msg_welfare <- sprintf('%s missing values in `welfare`.', sum(is.na(welfare)))
      }
      msg <- c(msg_main, i = msg_welfare, i = msg_weight)
    }
    rlang::inform(msg)

    # Clean vectors
    weight_no_na <- weight[!is.na(welfare)]
    weight_no_na <- weight_no_na[!is.na(weight_no_na)]
    welfare_no_na <- welfare[!is.na(weight)]
    welfare_no_na <- welfare_no_na[!is.na(welfare_no_na)]
    weight <- weight_no_na
    welfare <- welfare_no_na
  }

  # Remove negative values
  if (any(welfare < 0) | any(weight < 0)) {

    # Number of observations with negative values
    n_obs <- sum(welfare < 0 | weight < 0)

    # Info message
    if (n_obs == 1) {
      msg_main <- 'Info: Found 1 observation with negative values. This observation was removed.'
      if (sum(weight < 0) == 1) {
        msg_weight <- '1 negative value in `weight`.'
      } else {
        msg_weight <- sprintf('%s negative values in `weight`.', sum(weight < 0))
      }
      if (sum(welfare < 0) == 1) {
        msg_welfare <- '1 negative value in `welfare`.'
      } else {
        msg_welfare <- sprintf('%s negative values in `welfare`.', sum(weight < 0))
      }
      msg <- c(msg_main, i = msg_welfare, i = msg_weight)
    } else {
      msg_main <- 'Info: Found %s observations with negative values. These observations were removed.'
      msg_main <- sprintf(msg_main, n_obs)
      if (sum(weight < 0) == 1) {
        msg_weight <- '1 negative value in `weight`.'
      } else {
        msg_weight <- sprintf('%s negative values in `weight`.', sum(weight < 0))
      }
      if (sum(welfare < 0) == 1) {
        msg_welfare <- '1 negative value in `welfare`.'
      } else {
        msg_welfare <- sprintf('%s negative values in `welfare`.', sum(welfare < 0))
      }
      msg <- c(msg_main, i = msg_welfare, i = msg_weight)
    }
    rlang::inform(msg)

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

last_item <- function(x, word = "and") {
  if (!(is.character(x))) {
    warning("`x` must be character. coercing to character")
    x <- as.character(x)
  }

  lx <- length(x)
  if (lx == 1) {
    y <- x
  }
  else if (lx == 2) {
    y <- paste(x[1], word, x[2])
  }
  else {
    y <- c(x[1:lx - 1], paste(word, x[lx]))
    y <- paste(y, collapse = ", ")
  }
  return(y)
}

#' get_decimal_year_value
#'
#' Adjust auxiliary statistics for surveys that span multiple calendar years.
#' Values are adjusted by the weighted average of the years in question.
#'
#' @param year numeric: Year.
#' @param values numeric: A vector with two calendar year values.
#' @return numeric
#' @noRd
get_decimal_year_value <- function(year, values){
  weights <- get_weights(year)
  out <- stats::weighted.mean(x = values, w = weights)
  return(out)
}

#' get_weights
#'
#' In case the survey year spans two calendar years this helper function returns
#' the proportion of the survey year in each respective calendar year.
#'
#' @param survey_year numeric: A vector with survey years.
#' @return numeric
#' @noRd
get_weights <- function(year) {
  if (year %% 1 == 0) {
    out <- 1 # No need for weighted average for single years
  } else {
    weight2 <- year %% 1
    weight1 <- 1 - weight2
    out <- c(weight1, weight2)
  }
  return(out)
}
