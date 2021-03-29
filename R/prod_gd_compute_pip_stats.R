#' Computes poverty statistics (grouped)
#' Version used in prod
#'
#' Compute poverty statistics for grouped data by selecting the best functional
#' fit for the Lorenz curve (either beta or quadratic).
#'
#' @param welfare numeric: Cumulative proportion of welfare held by that
#' proportion of the population (Lorenz Curve).
#' @param povline numeric: Poverty line.
#' @param population numeric: Cumulative proportion of population.
#' @param requested_mean numeric: Welfare mean.
#' @param popshare numeric: Share of population living below the poverty line.
#' Optional.
#' @param default_ppp numeric: Default purchasing power parity.
#' @param ppp numeric: PPP request by user.
#' @param p0 numeric: **TO BE DOCUMENTED**.
#'
#' @return list
#' @keywords internal
prod_gd_compute_pip_stats <- function(welfare,
                                      povline,
                                      population,
                                      requested_mean,
                                      popshare = NULL,
                                      default_ppp = NULL,
                                      ppp = NULL,
                                      p0 = 0.5) {


  # Apply Lorenz quadratic fit ----------------------------------------------
  results_lq <- prod_gd_compute_pip_stats_lq(welfare         = welfare,
                                             population      = population,
                                             requested_mean  = requested_mean,
                                             povline         = povline,
                                             popshare        = popshare,
                                             default_ppp     = default_ppp,
                                             ppp             = ppp,
                                             p0              = p0)

  # Apply Lorenz beta fit ----------------------------------------------
  results_lb <- prod_gd_compute_pip_stats_lb(welfare         = welfare,
                                             population      = population,
                                             requested_mean  = requested_mean,
                                             povline         = povline,
                                             popshare        = popshare,
                                             default_ppp     = default_ppp,
                                             ppp             = ppp,
                                             p0              = p0)


  # Apply selection rules ---------------------------------------------------
  out <- gd_select_lorenz(lq = results_lq,
                          lb = results_lb)

  # Retun only subset of variables
  out <- out[c("poverty_line",
               "headcount",
               "poverty_gap",
               "poverty_severity",
               "watts")]


  return(out)
}
