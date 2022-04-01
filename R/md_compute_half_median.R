#' Compute the Half Median
#'
#' Compute the half median of the per day poverty line
#'
#' @param welfare numeric: A vector of consumption or income values
#' @param weight numeric: A vector of weights corresponding for each welfare value
#' @param threshold_rate numeric : A value between 0 and 1 for the proportion of
#' the median consumption/income for SPL calculation
#' @param ppp numeric: PPP values inputted by the user (set to 1 if welfare values are already in dollars)
#' @param cpi numeric: CPI values inputted by the user (set to 1 if welfare values year is the reference year)
#'
#' @return numeric
#' @keywords internal
#'
#' @examples
#' wbpip::md_compute_spl(welfare = 1:2000, weight = rep(1, 2000))


md_compute_half_median <- function(welfare,
                                   weight,
                                   ppp = 1,
                                   cpi = 1){

  ## setting up the data
  o       <- order(welfare)
  welfare <- welfare[o]
  weight  <- weight[o]

  dt <- data.table::data.table(welfare = welfare,
                               weight = weight)

  ## first compute the median using the SPL function
  half_median <- 0.5*(md_compute_spl(welfare = dt$welfare,
                                     weight = dt$weight,
                                     threshold_rate = 0.5,
                                     ppp = ppp,
                                     cpi = cpi))

  return(half_median)


}
