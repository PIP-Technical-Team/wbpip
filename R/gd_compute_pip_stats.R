#' Computes poverty statistics from grouped data
#'
#' @param welfare numeric: cumulative proportion of income held by that
#' proportion of the population (Lorenz Curve).
#' @param povline numeric: Poverty line
#' @param population numeric: cumulative proportion of population
#' @param requested_mean numeric: Welfare mean
#' @param popshare numeric: Share of population living below the poverty line.
#' Optional
#' @param default_ppp numeric: Default purchasing power parity
#' @param ppp numeric: PPP request by user
#' @param p0 numeric: To document
#'
#' @return list
#'
#' @export
#'
#
gd_compute_pip_stats <- function(welfare,
                                 povline,
                                 population,
                                 requested_mean,
                                 popshare = NULL,
                                 default_ppp = NULL,
                                 ppp = NULL,
                                 p0 = 0.5) {


  # Apply Lorenz quadratic fit ----------------------------------------------
  results_lq <- gd_compute_pip_stats_lq(welfare         = welfare,
                                        population      = population,
                                        requested_mean  = requested_mean,
                                        povline         = povline,
                                        popshare        = popshare,
                                        default_ppp     = default_ppp,
                                        ppp             = ppp,
                                        p0              = p0)

  # Apply Lorenz beta fit ----------------------------------------------
  results_lb <- gd_compute_pip_stats_lb(welfare         = welfare,
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
               "mean",
               "median",
               "headcount",
               "poverty_gap",
               "poverty_severity",
               "watts",
               "gini",
               "mld",
               "polarization",
               "deciles")]


  return(out)
}
