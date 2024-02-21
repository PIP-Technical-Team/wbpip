#' Wolfson polarization index
#'
#' Compute the Wolfson polarization index for microdata.
#'
#' @inheritParams md_compute_dist_stats
#' @inheritParams md_compute_gini
#' @param gini numeric: Gini. Output of [md_compute_gini()].
#' @param median numeric: Median. Output of [md_compute_quantiles()].
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
#'   mean = 950,
#'   median = 1000
#' )
#' @return numeric
#' @export
md_compute_polarization <- function(welfare, weight, gini,
                                    mean, median) {

  # Calculate poverty stats (for headcount and poverty gap)
  pov_stats <- md_compute_poverty_stats(
    welfare = welfare,
    weight = weight,
    povline_lcu = median
  )

  # Calculate mean for the bottom 50 %
  mean_below50 <-
    median *
      (1 - (pov_stats$poverty_gap / pov_stats$headcount))

  # Calculate distribution corrected mean
  dcm_mean <- (1 - gini) * mean

  # Calculate Wolfson polarization index
  # Formula: W = 2 * (dcm - mean_b50) / median
  # dcm = distribution corrected mean
  # mean_b50 = the mean of the poorest half
  polarization <- 2 * (dcm_mean - mean_below50) / median

  return(polarization)
}
