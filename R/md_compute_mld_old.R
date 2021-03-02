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
#' wbpip:::md_compute_mld_old(welfare = 1:2000, weight = rep(1, 2000))
#'
#' @keywords internal
md_compute_mld_old <- function(welfare, weight) {

  # Compute MLD
  weighted_welfare     <- weight * welfare
  sum_weighted_welfare <- sum(weighted_welfare)
  sum_weights          <- sum(weight)
  mean_welfare         <- sum_weighted_welfare / sum_weights
  mld                  <- 0

  for (i in seq_along(welfare)) {

    if (welfare[i] > 0) {
      # Apply Mean Log Deviation formula (MLD)
      # MLD is also referred to as GE(0) or Theil's L
      # See page 107 of https://openknowledge.worldbank.org/handle/10986/11985
      mld <- mld + weight[i] / sum_weights * log(mean_welfare / welfare[i])
    } else {
      # Use welfare = 1 as a proxy (instead of welfare = 0)
      # Otherwise mean_welfare / welfare is undefined
      mld <- mld + weight[i] / sum_weights * log(mean_welfare)
    }
  }

  return(mld)
}
