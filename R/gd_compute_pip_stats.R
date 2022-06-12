#' Computes poverty and inequality statistics (grouped)
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
#' @examples
#' # Compute PIP stats
#' res <- wbpip:::gd_compute_pip_stats(
#'          grouped_data_ex2$welfare,
#'          grouped_data_ex2$weight,
#'          requested_mean = 2.911786,
#'          povline = 1.9,
#'          default_ppp = 1)
#'
gd_compute_pip_stats <- function(welfare,
                                 povline,
                                 population,
                                 requested_mean,
                                 popshare = NULL,
                                 default_ppp = 1,
                                 ppp = NULL,
                                 p0 = 0.5) {


  # Apply Lorenz quadratic fit ----------------------------------------------
  results_lq <- gd_compute_pip_stats_lq(
    welfare = welfare,
    population = population,
    requested_mean = requested_mean,
    povline = povline,
    popshare = popshare,
    default_ppp = default_ppp,
    ppp = ppp,
    p0 = p0
  )

  # Apply Lorenz beta fit ----------------------------------------------
  results_lb <- gd_compute_pip_stats_lb(
    welfare = welfare,
    population = population,
    requested_mean = requested_mean,
    povline = povline,
    popshare = popshare,
    default_ppp = default_ppp,
    ppp = ppp,
    p0 = p0
  )


  # Apply selection rules ---------------------------------------------------
  out <- gd_select_lorenz(
    lq = results_lq,
    lb = results_lb
  )

  # Return only subset of variables
  out <- out[c(
    "poverty_line",
    "mean",
    "median",
    "headcount",
    "poverty_gap",
    "poverty_severity",
    "watts",
    "gini",
    "mld",
    "polarization",
    "deciles"
  )]


  return(out)
}


#' Handle missing ppp values
#'
#' @inheritParams gd_compute_pip_stats
#'
#' @return A list of two numeric values (ppp and requested_mean)

handle_missing_ppp <- function(ppp, requested_mean, default_ppp) {
  if (!is.null(ppp)) {
    requested_mean <- requested_mean * default_ppp / ppp
  } else {
    ppp <- default_ppp
  }
  return(list(ppp = ppp, requested_mean = requested_mean))
}
