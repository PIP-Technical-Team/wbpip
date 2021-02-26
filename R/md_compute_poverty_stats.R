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

  #--------- FGT measures ---------

  for (a in seq_along(alpha)) {

    y <- paste0("fgt", alpha[a])
    fgt <-  c(relative_distance^alpha[a],
              rep(0, collapse::fsum(!pov_status)))

    x <- collapse::fmean(x = fgt, w = weight)
    assign(y, x)

  }

  #--------- Watts index ---------

  sensitive_distance <- c(log(povline_lcu / welfare[pov_status]),
                          rep(0, collapse::fsum(!pov_status)))

  watts              <- collapse::fmean(x = sensitive_distance, w = weight)

  return(list(
    headcount        = fgt0,
    poverty_gap      = fgt1,
    poverty_severity = fgt2,
    watts = watts
  ))
}
