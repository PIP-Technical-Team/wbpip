#' Compute distributional statistics
#'
#' Given a vector of weights and welfare, this functions computes the various
#' distributional statistics.
#'
#' @inheritParams compute_pip_stats
#' @param mean numeric: A value with the mean. Optional.
#' @param lorenz data.frame: A table with points on the Lorenz curve. Output of
#'   `md_compute_lorenz()`. Optional.
#' @param nbins numeric: number of points on the Lorenz curve. Optional. Only
#'   used if `lorenz` is NULL
#' @param n_quantile numeric: Number of quantiles for which share of total
#'   income is desired. It can't be larger that the total number of percentiles
#'   in the Lorenz curve provided by the user. Default is 10.
#' @param ppp_year numeric PPP year - 2017 (default) or 2011
#'
#' @examples
#' wbpip:::md_compute_dist_stats(welfare = 1:2000, weight = rep(1, 2000))
#' @return data.frame
#' @keywords internal
md_compute_dist_stats <- function(welfare,
                                  weight,
                                  mean = NULL,
                                  nbins = NULL,
                                  lorenz = NULL,
                                  n_quantile = 10,
                                  ppp_year = 2017) {
  # Input Checks
  stopifnot(ppp_year %in% c(2017, 2011))

  if (is.null(mean)) {
    mean <- collapse::fmean(x = welfare, w = weight)
  }

  if (is.null(lorenz)) {
    lorenz <- md_compute_lorenz(
      welfare = welfare, weight = weight,
      nbins = nbins
    )
  }

  quantiles <- md_compute_quantiles(
    lwelfare    = lorenz[["lorenz_welfare"]],
    lweight     = lorenz[["lorenz_weight"]],
    percentile  = lorenz[["welfare"]]
  )
  median <- quantiles[["median"]]

  gini <- md_compute_gini(
    welfare = welfare,
    weight  = weight
  )

  mld <- md_compute_mld(
    welfare = welfare,
    weight  = weight,
    mean    = mean
  )

  polarization <- md_compute_polarization(
    welfare = welfare,
    weight  = weight,
    gini    = gini,
    mean    = mean,
    median  = median
  )

  spl <- md_compute_spl(
    welfare                 = welfare,
    weight                  = weight,
    weighted_median_welfare = median,
    ppp_year                = ppp_year
  )

  spr <- md_compute_fgt(
    welfare = welfare,
    weight  = weight,
    pl      = spl
  )

  return(list(
    mean         = mean,
    median       = median,
    gini         = gini,
    polarization = polarization,
    mld          = mld,
    quantiles    = quantiles[["quantiles"]],
    spl          = spl,
    spr          = spr
  ))
}
