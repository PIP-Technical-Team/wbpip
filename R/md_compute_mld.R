#' Mean Log Deviation
#'
#' Given a vector of weights and welfare, this functions computes the
#' Mean Log Deviation (MLD).
#'
#' @inheritParams md_compute_dist_stats
#' @inheritParams md_compute_gini
#' @return numeric
#' @examples
#' wbpip:::md_compute_mld(welfare = 1:2000, weight = rep(1, 2000))
#' @export
md_compute_mld <- function(welfare, weight, mean = NULL) {

  # Compute MLD
  if (is.null(mean)) {
    mean <- collapse::fmean(x = welfare, w = weight)
  }

  welfare[welfare <= 0] <- 1 # this should be done before the mean
  deviation <- log(mean / welfare)
  mld <- collapse::fmean(
    x = deviation,
    w = weight
  )
  return(mld)
}
