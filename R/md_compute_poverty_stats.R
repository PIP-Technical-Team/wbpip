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


  #--------- FGT Measures ---------
  fgt0 <- md_compute_fgt(welfare, weight, povline_lcu, 0)
  fgt1 <- md_compute_fgt(welfare, weight, povline_lcu, 1)
  fgt2 <- md_compute_fgt(welfare, weight, povline_lcu, 2)

  #--------- Watts index ---------
  pov_status         <- (welfare < povline_lcu)
  keep               <- welfare > 0 & pov_status
  w_gt_zero          <- welfare[keep]
  sensitive_distance <- log(povline_lcu / w_gt_zero)

  # watts              <- collapse::fmean(x = c(sensitive_distance, non_pov),
  #                                       w = weight[welfare > 0])
  #--------- Old Watts ---------

  weight_total <- sum(weight)
  watts <- sum(sensitive_distance * weight[keep]) /
    weight_total

  # Handle cases where Watts is numeric(0)
  if (identical(watts, numeric(0))) {
    watts <- 0
  }

  #--------- Prosperity Gap ---------
  # pg <- md_compute_prosperity_gap(
  #   welfare     = welfare,
  #   weight      = weight,
  #   povline_lcu = povline_lcu,
  #   cons_floor  = cons_floor
  # )

  #--------- Return ---------
  return(list(
    headcount        = fgt0,
    poverty_gap      = fgt1,
    poverty_severity = fgt2,
    watts            = watts #,
    # prosperity_gap   = pg
    # watts_old        = watts_old
  ))
}


#' Estimate FGT measures from microdata
#'
#' @inheritParams compute_pip_stats
#' @param pl numeric: poverty line
#' @param alpha  numeric: either 0, 1 or 2.
#'
#' @return numeric vector of length 1
#' @export
#'
#' @examples
#' md_compute_fgt(
#'   welfare = 1:2000,
#'   weight = rep(1, 2000),
#'   povline_lcu = 10,
#'   alpha = 0
#' )
md_compute_fgt <- function(welfare, weight, pl, alpha = 0) {

  pov_status <- (welfare < pl) * (1 - (welfare / pl)) ^ alpha

  collapse::fmean(pov_status, w = weight)

}
