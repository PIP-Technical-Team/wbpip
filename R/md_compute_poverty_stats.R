#' Compute Poverty Statistics
#'
#' Compute poverty statictics for microdata.
#'
#' Given a vector of consumption or income values and their respective weights
#' `md_compute_poverty_stats()` computes poverty headcount, poverty gap,
#' poverty severity and the watts index.
#'
#' @inheritParams compute_pip_stats
#' @param povline_lcu numeric: Poverty line in Local Currency Unit (LCU).
#'
#' @examples
#' wbpip:::md_compute_poverty_stats(
#'   welfare = 1:2000,
#'   weight = rep(1, 2000),
#'   povline_lcu = 10
#' )
#' @return list
#' @keywords internal
md_compute_poverty_stats <- function(welfare, weight, povline_lcu, cons_floor = 0.5) {

  pov_status <- (welfare < povline_lcu)
  relative_distance <- (1 - (welfare[pov_status] / povline_lcu))
  weight_pov <- weight[pov_status]
  weight_total <- sum(weight)

  #--------- FGT Measures ---------
  fgt0 <- sum(weight_pov) / weight_total
  fgt1 <- sum(relative_distance * weight_pov) / weight_total
  fgt2 <- sum(relative_distance^2 * weight_pov) / weight_total

  #--------- Watts index ---------
  keep <- welfare > 0 & pov_status
  w_gt_zero <- welfare[keep]
  sensitive_distance <- log(povline_lcu / w_gt_zero)

  # watts              <- collapse::fmean(x = c(sensitive_distance, non_pov),
  #                                       w = weight[welfare > 0])
  #--------- Old Watts ---------

  watts <- sum(sensitive_distance * weight[keep]) /
    weight_total

  # Handle cases where Watts is numeric(0)
  if (identical(watts, numeric(0))) {
    watts <- 0
  }

  #--------- Prosperity Gap ---------
  pg <- md_compute_prosperity_gap(
    welfare     = welfare,
    weight      = weight,
    povline_lcu = povline_lcu,
    cons_floor  = cons_floor
  )

  #--------- Return ---------
  return(list(
    headcount        = fgt0,
    poverty_gap      = fgt1,
    poverty_severity = fgt2,
    watts            = watts,
    prosperity_gap   = pg
    # watts_old        = watts_old
  ))
}
