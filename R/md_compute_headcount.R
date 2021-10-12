#' Compute Headcount
#'
#' Compute headcount for microdata.
#'
#' @param welfare numeric: A vector of income or consumption values.
#' @param weight numeric: A vector of weights.
#' @param povline_lcu numeric: Poverty line in Local Currency Unit (LCU).
#'
#' @examples
#' wbpip:::md_compute_headcount(
#'   welfare = 1:2000,
#'   weight = rep(1, 2000),
#'   povline_lcu = 10
#' )
#' @return numeric
#' @keywords internal
md_compute_headcount <- function(welfare, weight, povline_lcu) {

  pov_status <- (welfare < povline_lcu)
  weight_pov <- weight[pov_status]

  fgt0 <- sum(weight_pov) / sum(weight)

  return(fgt0)
}
