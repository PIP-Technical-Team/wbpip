#' Compute Poverty Statistics
#'
#' Compute poverty statictics for microdata.
#'
#' Given a vector of consumption or income values and their respective weights
#' `md_compute_poverty_stats()` computes poverty headcount, poverty gap,
#' poverty severity and the watts index.
#'
#' @param welfare numeric: A vector of income or consumption values.
#' @param weight numeric: A vector of weights.
#' @param povline_lcu numeric: Poverty line in Local Currency Unit (LCU).
#'
#' @examples
#' wbpip:::md_compute_poverty_stats(
#'   welfare = 1:2000,
#'   weight = rep(1, 2000),
#'   povline_lcu = 10)
#'
#' @return list
#' @keywords internal
md_compute_poverty_stats <- function(welfare, weight, povline_lcu) {

  headcount <- 0
  gap <- 0
  severity <- 0
  watt8 <- 0

  for (i in seq_along(welfare)) {

    weight_i <- weight[i]
    welfare_i <- welfare[i]

    if (welfare_i <= povline_lcu) {

      headcount <- sum(headcount, weight_i)
      gap_i <- 1 - welfare_i / povline_lcu
      gap <- sum(gap, weight_i * gap_i)
      severity <- sum(severity, weight_i * gap_i ^ 2)
      if (welfare_i > 0) { # Is this check needed no negative welfare value should make it to the application
        watt8 <- sum(watt8, weight_i * log(povline_lcu / welfare_i))
      }

    }
  }

  #compute the values for the return
  sum_weight <- sum(weight)

  headcount <- headcount / sum_weight
  gap <- gap / sum_weight
  severity <- severity / sum_weight
  watt8 <- if (headcount > 0) {
    watt8 <- watt8 / sum_weight
  } else {
      watt8 <- 0}

  return(list(
    headcount = headcount,
    poverty_gap = gap,
    poverty_severity = severity,
    watts = watt8
  ))
}
