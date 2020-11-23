#' md_compute_mld
#' Given a vector of weights and welfare, this functions computes the
#' Mean Log Deviation (MLD)
#' @param welfare numeric: vector of welfare measures
#' @param weight numeric: vector of weights
#'
#' @return numeric
#' @export
#'
#' @examples
#' md_compute_mld(welfare = 1:2000, weight = 1:2000)
#'
#' data("md_ABC_2000_income")
#' md_compute_mld(md_ABC_2000_income$welfare, md_ABC_2000_income$weight)

md_compute_mld <- function(welfare,
                           weight = NULL) {

  # Set all weights to 1 if none are supplied
  if (is.null(weight)) weight <- rep(1, length(welfare))

  # Make sure data is sorted
  ordy    <- order(welfare)   # order of welfare
  welfare <- welfare[ordy]    #order weight
  weight  <- weight[ordy]     # order welfare

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
