#' Compute distributionla statistics
#'
#' Given a vector of weights and welfare, this functions computes the
#' various distributional statistics
#' @param welfare numeric: vector of welfare measures
#' @param weight numeric: vector of weights
#' @param nbins numeric: number of points on the Lorenz curve
#' @param n_quantile numeric: Number of quantiles for which share of total income
#' is desired. It can't be larger that the total number of percentiles in the
#' Lorenz curve provided by the user.  default is 10.
#' @return data.frame
#'
#' #' @examples#'
#' data("md_ABC_2000_income")
#' md_compute_dist_stats(md_ABC_2000_income$welfare, md_ABC_2000_income$weight)
#'
#' @export
#'
md_compute_dist_stats <- function(welfare,
                                  weight = NULL,
                                  nbins  = NULL,
                                  n_quantile = 10) {

  # set all weights to 1 if none are supplied
  if (is.null(weight) == TRUE) {
    weight <- rep(1, length(welfare))
  }

  # make sure the data is sorted
  ordy    <- order(welfare)   # order of welfare
  welfare <- welfare[ordy]    #order weight
  weight  <- weight[ordy]     # order welfare

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
