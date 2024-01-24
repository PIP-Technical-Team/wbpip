#' Computes poverty statistics (grouped, prod)
#'
#' Compute poverty statistics for grouped data by selecting the best functional
#' fit for the Lorenz curve (either beta or quadratic).
#'
#' Version used in production.
#'
#' @inheritParams gd_compute_pip_stats
#' @return list
#' @keywords internal
prod_gd_compute_pip_stats <- function(welfare,
                                      povline,
                                      population,
                                      mean,
                                      svy_median_lcu,
                                      svy_median_ppp,
                                      popshare = NULL,
                                      default_ppp,
                                      ppp = NULL,
                                      p0 = 0.5) {

  # Adjust mean and median if different PPP value is provided
  if (!is.null(ppp)) {
    mean <- mean * default_ppp / ppp
    median <- svy_median_lcu * default_ppp / ppp
  } else {
    ppp <- default_ppp
    median <- svy_median_ppp
  }

   # Apply Lorenz quadratic fit ----------------------------------------------
  results_lq <- prod_gd_compute_pip_stats_lq(
    welfare = welfare,
    population = population,
    mean = mean,
    povline = povline,
    popshare = popshare,
    default_ppp = default_ppp,
    ppp = ppp,
    p0 = p0
  )

  # Apply Lorenz beta fit ----------------------------------------------
  results_lb <- prod_gd_compute_pip_stats_lb(
    welfare = welfare,
    population = population,
    mean = mean,
    povline = povline,
    popshare = popshare,
    default_ppp = default_ppp,
    ppp = ppp,
    p0 = p0
  )


  # Apply selection rules ---------------------------------------------------
  out <- prod_gd_select_lorenz(
    lq = results_lq,
    lb = results_lb
  )

  # Add median to response
  out$median <- median

  # Return only subset of variables
  out <- out[c(
    "poverty_line",
    "mean",
    "median",
    "headcount",
    "poverty_gap",
    "poverty_severity",
    "watts"
  )]


  return(out)
}
