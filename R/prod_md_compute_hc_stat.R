#' Compute default PIP statistics
#' Version used in production. Does not re-compute all distributional stats.
#' Distributional stats are pre-computed as they are not sensitive to the poverty line.
#'
#' Compute poverty and distributional statistics for microdata.
#'
#' @param welfare numeric: A vector of income or consumption values.
#' @param povline numeric: Monthly poverty line in international dollars.
#' @param population numeric: A vector of population weights, optional, a vector
#' of 1s if not specified.
#' @param requested_mean numeric: Welfare mean in international dollars
#' @param svy_mean_lcu numeric: Welfare mean in Local Currency Unit
#' @param popshare numeric: Share of population for which the corresponding
#' quantile is desired. Default .5 (i.e., weighted median).
#' @param default_ppp numeric: Default purchasing power parity.
#' @param ppp numeric: PPP requested by user.
#'
#' @return list
#' @keywords internal
prod_md_compute_hc_stat <- function(welfare,
                                    povline,
                                    population = NULL,
                                    requested_mean = NULL,
                                    svy_mean_lcu = NULL,
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
  } else {
    mean <- requested_mean
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
  headcount <- md_compute_headcount(
    welfare = welfare,
    povline_lcu = adjusted_povline[["povline_lcu"]],
    weight = population
  )

  return(list(headcount = headcount))
}
