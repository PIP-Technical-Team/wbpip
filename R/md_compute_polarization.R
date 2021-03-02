#' Wolfson polarization index
#'
#' Compute the Wolfson polarization index for microdata.
#'
#' @param welfare numeric: A vector of income or consumption values.
#' @param weight numeric: A vector of weights.
#' @param gini numeric: Gini. Output of [md_compute_gini()].
#' @param weighted_mean numeric: Mean.
#' @param weighted_median numeric: Median. Output of [md_compute_quantiles()].
#'
#' @references
#' Ravallion, M., S. Chen. 1996.
#' "[What Can New Survey Data Tell Us about Recent Changes in Distribution and Poverty?](http://documents1.worldbank.org/curated/en/202781468739531561/pdf/multi-page.pdf)".
#' Policy Research Working Paper 1694.
#' World Bank, Washington, DC.
#'
#' @examples
#' wbpip:::md_compute_polarization(
#'   welfare = 1:2000,
#'   weight = rep(1, 2000),
#'   gini = 0.4,
#'   weighted_mean = 950,
#'   weighted_median = 1000)
#'
#' @return numeric
#' @keywords internal
md_compute_polarization <- function(welfare, weight, gini,
                                    weighted_mean,
                                    weighted_median) {

  # Calculate poverty stats (for headcount and poverty gap)
  pov_stats <- md_compute_poverty_stats(welfare = welfare,
                                        weight = weight,
                                        povline_lcu = weighted_median)

  # Calculate mean for the bottom 50 %
  mean_below50 <-
    weighted_median *
    (1 - (pov_stats$poverty_gap / pov_stats$headcount))

  # Calculate distribution corrected mean
  dcm_mean <- (1 - gini) * weighted_mean

  # Calculate Wolfson polaratisation index
  # Formula: W = 2 * (dcm - mean_b50) / median
  # dcm = distribution corrected mean
  # mean_b50 = the mean of the poorest half
  polarization <- 2 * (dcm_mean - mean_below50) /  weighted_median

  return(polarization)
}

