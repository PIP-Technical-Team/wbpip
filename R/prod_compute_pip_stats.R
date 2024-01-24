#' Compute default PIP statistics (microdata, prod)
#'
#' Compute poverty and distributional statistics for microdata.
#'
#' Version used in production. Does not re-compute all distributional stats.
#' Distributional stats are pre-computed as they are not sensitive to the poverty line.
#'
#' @param svy_mean_lcu numeric: Welfare mean in Local Currency Unit
#' @param svy_median_lcu numeric: Welfare median in Local Currency Unit
#' @param svy_median_ppp numeric: Welfare median in PPP
#' @inheritParams compute_pip_stats
#'
#' @return list
prod_compute_pip_stats <- function(welfare,
                                   povline,
                                   population = NULL,
                                   mean = NULL,
                                   svy_mean_lcu = NULL,
                                   svy_median_lcu,
                                   svy_median_ppp,
                                   popshare = NULL,
                                   default_ppp,
                                   ppp = NULL,
                                   p0 = 0.5,
                                   distribution_type = c(
                                     "micro",
                                     "group",
                                     "aggregate",
                                     "imputed"
                                   )) {
  distribution_type <- match.arg(distribution_type)

  if (distribution_type %in% c("micro", "imputed")) {
    out <- prod_md_compute_pip_stats(
      welfare = welfare,
      povline = povline,
      population = population,
      mean = mean,
      svy_mean_lcu = svy_mean_lcu,
      svy_median_lcu = svy_median_lcu,
      svy_median_ppp = svy_median_ppp,
      popshare = popshare,
      default_ppp = default_ppp,
      ppp = ppp
    )

    return(out)
  } else if (distribution_type %in% c("group", "aggregate")) {
    out <- prod_gd_compute_pip_stats(
      welfare = welfare,
      povline = povline,
      population = population,
      mean = mean,
      svy_median_lcu = svy_median_lcu,
      svy_median_ppp = svy_median_ppp,
      popshare = popshare,
      default_ppp = default_ppp,
      ppp = ppp,
      p0 = p0
    )

    return(out)
  } else {
    return(NA_real_)
  }
}
