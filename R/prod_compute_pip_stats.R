#' Compute default PIP statistics (microdata)
#' Version used in production. Does not re-compute all distributional stats.
#' Distributional stats are pre-computed as they are not sensitive to the poverty line.
#'
#' Compute poverty and distributional statistics for microdata.
#'
#' @param welfare numeric: A vector of income or consumption values
#' @param povline numeric: Monthly poverty line in international dollars
#' @param population numeric: A vector of population weights, optional, a vector
#' of 1s if not specified.
#' @param requested_mean numeric: Welfare mean in international dollars
#' @param svy_mean_lcu numeric: Welfare mean in Local Currency Unit
#' @param popshare numeric: Share of population for which the corresponding
#' quantile is desired. Default .5 (i.e., weighted median)
#' @param default_ppp numeric: Default purchasing power parity
#' @param ppp numeric: PPP requested by user
#' @param distribution_type character: Type of distribution, either micro,
#'   group, aggregate or imputed.
#'
#' @return list
prod_compute_pip_stats <- function(welfare,
                                   povline,
                                   population = NULL,
                                   requested_mean = NULL,
                                   svy_mean_lcu = NULL,
                                   popshare = NULL,
                                   default_ppp = 1,
                                   ppp = NULL,
                                   distribution_type = c("micro",
                                                         "group",
                                                         "aggregate",
                                                         "imputed")) {

  distribution_type <- match.arg(distribution_type)

  if (distribution_type == "micro") {

    out <- prod_md_compute_pip_stats(welfare        = welfare,
                                     povline        = povline,
                                     population     = population,
                                     requested_mean = requested_mean,
                                     svy_mean_lcu   = svy_mean_lcu,
                                     popshare       = popshare,
                                     default_ppp    = default_ppp,
                                     ppp            = ppp)

    return(out)

  } else if (distribution_type == "group") {

    out <- prod_gd_compute_pip_stats(welfare        = welfare,
                                     povline        = povline,
                                     population     = population,
                                     requested_mean = requested_mean,
                                     popshare       = popshare,
                                     default_ppp    = default_ppp,
                                     ppp            = ppp)

    return(out)

  } else if (distribution_type == "aggregate") {

    return(NA)

  } else if (distribution_type == "imputed") {

    return(NA)

  } else {

    return(NA)
  }
}
