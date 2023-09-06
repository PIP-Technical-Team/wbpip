#' Compute default PIP statistics (microdata)
#'
#' Compute poverty and distributional statistics for microdata.
#'
#' @param welfare numeric: A vector of income or consumption values
#' @param povline numeric: Poverty line in international dollars
#' @param population numeric: A vector of population weights, optional, a vector
#' of 1s if not specified.
#' @param requested_mean numeric: Welfare mean in international dollars
#' @param popshare numeric: Share of population for which the corresponding
#' quantile is desired. Default .5 (i.e., weighted median)
#' @param default_ppp numeric: Default purchasing power parity
#' @param ppp numeric: PPP requested by user
#' @param p0 numeric: TO be documented
#' @param ppp_year numeric: PPP year - 2017 (default) or 2011
#' @param cons_floor numeric: gives the consumption floor. Default is $0.5 per day, if NULL then no floor.
#' @param distribution_type character: Type of distribution, either micro,
#'   group, aggregate or imputed.
#'
#' @return list
#' @export
compute_pip_stats <- function(welfare,
                              povline,
                              population = NULL,
                              requested_mean = NULL,
                              popshare = NULL,
                              default_ppp = 1,
                              ppp = NULL,
                              p0 = 0.5,
                              ppp_year = c(2017, 2011),
                              cons_floor = c(0.5),
                              distribution_type = c(
                                "micro",
                                "group",
                                "aggregate",
                                "imputed"
                              )) {
  # Input checks
  distribution_type <- match.arg(distribution_type)
  censor_type       <- match.arg(censor_type)
  stopifnot(ppp_year %in% c(2017, 2011))

  if (distribution_type == "micro") {
    out <- md_compute_pip_stats(
      welfare        = welfare,
      povline        = povline,
      population     = population,
      requested_mean = requested_mean,
      popshare       = popshare,
      ppp_year       = ppp_year,
      default_ppp    = default_ppp,
      ppp            = ppp,
      cons_floor     = cons_floor
    )

    return(out)
  } else if (distribution_type %in% c("group", "aggregate")) {
    out <- gd_compute_pip_stats(
      welfare         = welfare,
      povline         = povline,
      population      = population,
      requested_mean  = requested_mean,
      popshare        = popshare,
      ppp_year        = ppp_year,
      default_ppp     = default_ppp,
      ppp             = ppp,
      p0              = p0
    )

    return(out)
  } else if (distribution_type == "imputed") {
    return(NA)
  } else {
    return(NA)
  }
}
