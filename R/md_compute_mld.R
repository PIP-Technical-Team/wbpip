#' Mean Log Deviation
#'
#' Given a vector of weights and welfare, this functions computes the
#' Mean Log Deviation (MLD).
#'
#' @param welfare numeric: vector of welfare measures
#' @param weight numeric: vector of weights
#'
#' @return numeric
#'
#' @examples
#' wbpip:::md_compute_mld(welfare = 1:2000, weight = rep(1, 2000))
#'
#' @keywords internal
md_compute_mld <- function(welfare, weight) {

  # Compute MLD
  mean_welfare         <- collapse::fmean(x = welfare,
                                         w = weight)
  welfare[welfare <= 0] <- 1 # this should be done before the mean
  deviation             <- log(mean_welfare/welfare)
  mld                   <- collapse::fmean(x = deviation,
                                          w = weight)
  return(mld)
}

