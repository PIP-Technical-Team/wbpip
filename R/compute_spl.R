#' Compute SPL
#'
#' Compute the societal poverty line based on the median welfare in PPP-adjusted
#' dollars.
#'
#' @param ppp_year numeric: PPP year.
#' @param weighted_median_ppp numeric: Median. Weighted median in PPP terms.
#'
#' @references D. Jolliffe, E. B. Prydz. 2017. "[Societal Poverty: A Relative
#' and Relevant
#' Measure](https://documents1.worldbank.org/curated/en/133671495562984832/pdf/Societal-poverty-a-relative-and-relevant-measure.pdf)".
#' Policy Research Working Paper 8073. World Bank, Washington, DC.
#'
#' @return numeric vector with spl values
#' @keywords internal
compute_spl <- function(weighted_median_ppp,
                        ppp_year) {

  threshold_rate <-  0.5

  if (ppp_year == 2011) {
    constant  <- 1
    min_level <- 1.9
  } else if (ppp_year == 2017) {
    constant  <- 1.15
    min_level <- 2.15
  } else if (ppp_year == 2021) {
    constant  <- 1.3
    min_level <- 3
  } else {
    cli::cli_abort("Invalid PPP year. {.field ppp_year} must be either 2011 or 2017")
  }

  # Calculate SPL according to threshold rate
  spl <- constant + threshold_rate * weighted_median_ppp

  # Set minimum level if needed
  spl[spl < min_level] <- min_level

  return(spl)

}
