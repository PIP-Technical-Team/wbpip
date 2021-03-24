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
#' @param popshare numeric: Share of population for which the corresponding
#' quantile is desired. Default .5 (i.e., weighted median).
#' @param default_ppp numeric: Default purchasing power parity.
#' @param ppp numeric: PPP requested by user.
#'
#' @return list
#' @keywords internal
prod_md_compute_pip_stats <- function(welfare,
                                      povline,
                                      population = NULL,
                                      requested_mean = NULL,
                                      popshare = NULL,
                                      default_ppp = 1,
                                      ppp = NULL) {

  # Take care of potentially undefined values
  if (is.null(ppp)) {ppp <- default_ppp}
  # if (is.null(requested_mean)) {requested_mean <- dist_stats[["mean"]]}
  # data_mean <- dist_stats[["mean"]]
  # data_mean <- svy_mean # To be implemented for interpolation
  data_mean <- requested_mean

  # Adjust values to account for PPP or welfare mean change
  mean <- requested_mean * ppp / default_ppp

  # Retrieve poverty line in Local Currency Unit (LCU)
  adjusted_povline <- md_compute_povline_lcu(welfare = welfare,
                                             povline = povline,
                                             weight  = population,
                                             popshare = popshare,
                                             requested_mean = mean,
                                             data_mean = data_mean)
  # Compute poverty stats
  pov_stats <- md_compute_poverty_stats(welfare = welfare,
                                        povline_lcu = adjusted_povline[["povline_lcu"]],
                                        weight  = population)

  return(list(
    poverty_line     = adjusted_povline[["povline"]],
    mean             = mean,
    headcount        = pov_stats[["headcount"]],
    poverty_gap      = pov_stats[["poverty_gap"]],
    poverty_severity = pov_stats[["poverty_severity"]],
    watts            = pov_stats[["watts"]]
  ))
}
