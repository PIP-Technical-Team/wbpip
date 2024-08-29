#' Compute default PIP statistics (prod)
#'
#' Compute poverty and distributional statistics for microdata.
#'
#' Version used in production. Does not re-compute all distributional stats.
#' Distributional stats are pre-computed as they are not sensitive to the poverty line.
#'
#' @param svy_mean_lcu numeric: Welfare mean in Local Currency Unit
#' @inheritParams md_compute_pip_stats
#' @return list
#' @keywords internal
prod_md_compute_pip_stats <- function(welfare,
                                      povline,
                                      population = NULL,
                                      requested_mean = NULL,
                                      svy_mean_lcu = NULL,
                                      svy_median_lcu,
                                      svy_median_ppp,
                                      popshare = NULL,
                                      default_ppp,
                                      ppp = NULL) {

  # Take care of potentially undefined values
  if (is.null(requested_mean)) {
    requested_mean <- svy_mean_lcu
  }

  # Adjust mean and median if different PPP value is provided
  if (!is.null(ppp)) {
    mean <- requested_mean * default_ppp / ppp
    median <- svy_median_lcu * default_ppp / ppp
  } else {
    mean <- requested_mean
    median <- svy_median_ppp
  }

  # Retrieve poverty line in Local Currency Unit (LCU)
  adjusted_povline <- md_compute_povline_lcu(
    welfare = welfare,
    povline = povline,
    weight = population,
    popshare = popshare,
    requested_mean = mean,
    data_mean = svy_mean_lcu
  )
  # Compute poverty stats
  pov_stats <- md_compute_poverty_stats(
    welfare = welfare,
    povline_lcu = adjusted_povline[["povline_lcu"]],
    weight = population
  )
  browser()
  return(list(
    poverty_line     = adjusted_povline[["povline"]],
    mean             = mean,
    median           = median,
    headcount        = pov_stats[["headcount"]],
    poverty_gap      = pov_stats[["poverty_gap"]],
    poverty_severity = pov_stats[["poverty_severity"]],
    watts            = pov_stats[["watts"]]
  ))
}
