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
                                 requested_mean = NULL,
                                 popshare = NULL,
                                 default_ppp = 1,
                                 ppp = NULL,
                                 ppp_year = 2017,
                                 cons_floor = 0.5) {

  stopifnot(ppp_year %in% c(2017, 2011))

  # Compute distributional statistics
  dist_stats <- md_compute_dist_stats(
    welfare  = welfare,
    weight   = population,
    ppp_year = ppp_year
  )

  # Take care of potentially undefined values
  if (is.null(ppp)) {
    ppp <- default_ppp
  }
  if (is.null(requested_mean)) {
    requested_mean <- dist_stats[["mean"]]
  }
  data_mean <- dist_stats[["mean"]]

  # Adjust values to account for PPP or welfare mean change
  mean <- requested_mean * default_ppp / ppp
  median <- dist_stats[["median"]] / (data_mean / requested_mean)

  # Retrieve poverty line in Local Currency Unit (LCU)
  adjusted_povline <- md_compute_povline_lcu(
    welfare = welfare,
    povline = povline,
    weight = population,
    popshare = popshare,
    requested_mean = mean,
    data_mean = data_mean
  )
  # Compute poverty stats
  pov_stats <- md_compute_poverty_stats(
    welfare     = welfare,
    povline_lcu = adjusted_povline[["povline_lcu"]],
    weight      = population,
    cons_floor  = cons_floor
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
    deciles          = dist_stats[["quantiles"]],
    spl              = dist_stats[["spl"]],
    spr              = dist_stats[["spr"]]
  ))
}
