#' get_decimal_year_value
#'
#' Adjust auxiliary statistics for surveys that span multiple calendar years.
#' Values are adjusted by the weighted average of the years in question.
#'
#' @param year numeric: Year.
#' @param values numeric: A vector with two calendar year values.
#' @return numeric
#' @noRd
get_decimal_year_value <- function(year, values) {
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
