#' Wolfson polarization index
#'
#' Compute the Wolfson polarization index for microdata.
#'
#' @param welfare numeric: A vector of income or consumption values.
#' @param weight numeric: A vector of weights, optional.
#'
#' @references
#' Ravallion, M., Chen, S. 1996.
#' "[What Can New Survey Data Tell Us about Recent Changes in Distribution and Poverty?](http://documents1.worldbank.org/curated/en/202781468739531561/pdf/multi-page.pdf)".
#' _Policy Research Working Paper 1694_. Washington, DC: World Bank.
#'
#' @examples
#' # Simple example
#' md_compute_polarization(welfare = 1:10)
#'
#' # Include weights
#' md_compute_polarization(welfare = 1:10, weight = 1:10)
#'
#' # Microdata example
#' data("md_ABC_2000_income")
#' md_compute_polarization(md_ABC_2000_income$welfare, md_ABC_2000_income$weight)
#'
#' @export
md_compute_polarization <- function(welfare, weight = NULL) {

  # Set all weights to 1 if none are supplied
  if (is.null(weight)) weight <- rep(1, length(welfare))

  # Make sure data is sorted
  ordy    <- order(welfare)   # order of welfare
  welfare <- welfare[ordy]    # order weight
  weight  <- weight[ordy]     # order welfare

  # Calculate weighted mean
  weighted_mean <- stats::weighted.mean(x = welfare, w = weight)

  # Calculate Gini
  gini <- md_compute_gini(welfare = welfare, weight = weight)

  # Calculate Lorenz
  lz <- md_compute_lorenz(welfare = welfare, weight = weight)

  # Calculate weighted median
  weighted_median <- md_compute_quantiles(
    lwelfare = lz$lorenz_welfare,
    lweight = lz$lorenz_weight,
    percentile = lz$welfare)[['median']]

  # Calculate poverty stats (for headcount and poverty gap)
  pov_stats <- md_compute_poverty_stats(welfare = welfare,
                                        weight = weight,
                                        povline = weighted_median)

  # Calculate mean for the bottom 50 %
  mean_below50 <- weighted_median * (1 - pov_stats$poverty_gap / pov_stats$headcount)

  # Calculate distribution corrected mean
  dcm_mean <- (1 - gini) * weighted_mean

  # Calculate Wolfson polaratisation index
  # Formula: W = 2 * (dcm - mean_b50) / median
  # dcm = distribution corrected mean
  # mean_b50 = the mean of the poorest half
  polarization <- 2 * (dcm_mean - mean_below50) /  weighted_median

  return(polarization)
}

