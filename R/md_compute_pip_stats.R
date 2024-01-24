#' Compute default PIP statistics
#'
#' Compute poverty and distributional statistics for microdata.
#'
#' @inheritParams compute_pip_stats
#' @return list
#' @keywords internal
md_compute_pip_stats <- function(welfare,
                                 povline,
                                 population,
                                 mean = NULL,
                                 popshare = NULL,
                                 default_ppp = 1,
                                 ppp = NULL) {

  # Compute distributional statistics
  dist_stats <- md_compute_dist_stats(
    welfare = welfare,
    weight = population
  )

  # Take care of potentially undefined values
  if (is.null(ppp)) {
    ppp <- default_ppp
  }
  if (is.null(mean)) {
    mean <- dist_stats[["mean"]]
  }
  data_mean <- dist_stats[["mean"]]

  # Adjust values to account for PPP or welfare mean change
  mean <- mean * default_ppp / ppp
  median <- dist_stats[["median"]] / (data_mean / mean)

  # Retrieve poverty line in Local Currency Unit (LCU)
  adjusted_povline <- md_compute_povline_lcu(
    welfare = welfare,
    povline = povline,
    weight = population,
    popshare = popshare,
    mean = mean,
    data_mean = data_mean
  )
  # Compute poverty stats
  pov_stats <- md_compute_poverty_stats(
    welfare = welfare,
    povline_lcu = adjusted_povline[["povline_lcu"]],
    weight = population
  )

  return(list(
    poverty_line     = adjusted_povline[["povline"]],
    mean             = mean,
    median           = median,
    headcount        = pov_stats[["headcount"]],
    poverty_gap      = pov_stats[["poverty_gap"]],
    poverty_severity = pov_stats[["poverty_severity"]],
    watts            = pov_stats[["watts"]],
    gini             = dist_stats[["gini"]],
    mld              = dist_stats[["mld"]],
    polarization     = dist_stats[["polarization"]],
    deciles          = dist_stats[["quantiles"]]
  ))
}
