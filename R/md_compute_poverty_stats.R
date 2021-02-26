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

  alpha             <- c(0, 1, 2)
  pov_status        <- (welfare < povline_lcu)
  relative_distance <- (1 - (welfare[pov_status] / povline_lcu))
  non_pov           <- rep(0, collapse::fsum(!pov_status))

  fgt0 <- collapse::fmean(x = pov_status, w = weight)

  fgt1 <- collapse::fmean(x = c(relative_distance, non_pov), w = weight)

  fgt2 <- collapse::fmean(x = c(relative_distance^2, non_pov), w = weight)

  #--------- Watts index ---------

  sensitive_distance <- c(log(povline_lcu / welfare[pov_status]), non_pov)

  watts              <- collapse::fmean(x = sensitive_distance, w = weight)

  return(list(
    headcount        = fgt0,
    poverty_gap      = fgt1,
    poverty_severity = fgt2,
    watts = watts
  ))
}
