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

check_numeric <- function(x) {

  msg <- "`x` must be a numeric vector"
  hint <- sprintf("You've supplied a %s vector.", class(x))

  if (!is.numeric(x))
    rlang::abort(c(
      msg,
      i = hint
    ))
}

check_equal_length <- function(x, y) {

  len_x <- length(x)
  len_y <- length(y)

  msg <- '`x` and `y` must be of the same length'
  hint <- sprintf("`x` is of length %s and `y` is of length %s",
                  len_x,
                  len_y)

  if (len_x != len_y)
    rlang::abort(c(
      msg,
      i = hint
    ))
}
