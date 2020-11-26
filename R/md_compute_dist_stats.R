#' Compute distributional statistics
#'
#' Given a vector of weights and welfare, this functions computes the various
#' distributional statistics.
#'
#' @param welfare numeric: A vector of income or consumption values.
#' @param weight numeric: A vector of weights.
#' @param nbins numeric: number of points on the Lorenz curve.
#' @param n_quantile numeric: Number of quantiles for which share of total
#'   income is desired. It can't be larger that the total number of percentiles
#'   in the Lorenz curve provided by the user. Default is 10.
#' @examples
#' wbpip:::md_compute_dist_stats(welfare = 1:2000, weight = rep(1, 2000))
#'
#' @return data.frame
#' @keywords internal
md_compute_dist_stats <- function(welfare, weight,
                                  nbins  = NULL,
                                  n_quantile = 10) {

  gini <- md_compute_gini(welfare = welfare,
                          weight  = weight)

  lorenz <- md_compute_lorenz(welfare = welfare,
                              weight  = weight,
                              nbins   = nbins)

  quantiles <- md_compute_quantiles(lwelfare   = lorenz[["lorenz_welfare"]],
                                    lweight    = lorenz[["lorenz_weight"]],
                                    percentile = lorenz[["welfare"]])

  mean <- stats::weighted.mean(welfare, weight)

  mld <- md_compute_mld(welfare = welfare,
                        weight  = weight)

  return(list(
    mean = mean,
    median = quantiles[["median"]],
    gini = gini,
    polarization = NA,
    mld = mld,
    quantiles = quantiles[["quantiles"]]
  ))
}
