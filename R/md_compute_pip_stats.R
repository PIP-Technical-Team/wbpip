#' Compute default PIP statistics from microdata
#'
#' Compute poverty and distributional statistics from microdata records
#'
#' @param welfare numeric: A vector of income or consumption values
#' @param povline numeric: Monthly poverty line in international dollars
#' @param weight numeric: A vector of weights, optional, a vector of 1s if not specified.
#' @param ppp numeric: PPP requested by user
#' @param default_ppp numeric: Default purchasing power parity
#' @param requested_mean numeric: Welfare mean in international dollars
#' @param popshare numeric: Share of population for which the corresponding
#' quantile is desired. Default .5 (i.e., weighted median)
#'
#' @return list
#' @export
#'
md_compute_pip_stats <- function(welfare,
                                 povline,
                                 weight = NULL,
                                 ppp = NULL,
                                 default_ppp = 1,
                                 requested_mean = NULL,
                                 popshare = NULL) {

  # set all weights to 1 if none are supplied
  if (is.null(weight) == TRUE) {
    weight <- rep(1, length(welfare))
  }

  # make sure the data is sorted
  ordy    <- order(welfare)   # order of welfare
  welfare <- welfare[ordy]    #order weight
  weight  <- weight[ordy]     # order welfare

  # Compute distributional statistics
  dist_stats <- md_compute_dist_stats(welfare = welfare,
                                      weight  = weight)

  # Take care of potentially undefined values
  if (is.null(ppp)) {ppp <- default_ppp}
  if (is.null(requested_mean)) {requested_mean <- dist_stats[["mean"]]}
  data_mean <- dist_stats[["mean"]]

  # Adjust values to account for PPP or welfare mean change
  mean <- requested_mean * ppp / default_ppp
  median <- dist_stats[["median"]] / (data_mean / requested_mean)

  # Retrieve poverty line in Local Currency Unit (LCU)
  adjusted_povline <- md_compute_povline_lcu(welfare = welfare,
                                             povline = povline,
                                             weight  = weight,
                                             popshare = popshare,
                                             requested_mean = mean,
                                             data_mean = data_mean)
  # Compute poverty stats
  pov_stats <- md_compute_poverty_stats(welfare = welfare,
                                        povline_lcu = adjusted_povline[["povline_lcu"]],
                                        weight  = weight)

  return(list(
    poverty_line = adjusted_povline[["povline"]],
    mean = mean,
<<<<<<< HEAD
    median = median,
=======
    meadian = median,
>>>>>>> 86b110647ac3f9d9d709fba9e20ec7904488d6c6
    headcount = pov_stats[["headcount"]],
    poverty_gap = pov_stats[["poverty_gap"]],
    poverty_severity = pov_stats[["poverty_severity"]],
    watts = pov_stats[["watts"]],
    gini = dist_stats[["gini"]],
    mld = dist_stats[["mld"]],
    polarization = dist_stats[["polarization"]],
    deciles = dist_stats[["quantiles"]]
  ))
}
