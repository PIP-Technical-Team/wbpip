#' Compute Societal Poverty Line (SPL)
#'
#' Compute SPL based off median welfare in 2017 or 2011 PPP terms
#'
#' @param welfare numeric welfare vector
#' @param weight numeric weight vector
#' @param weighted_median_welfare numeric weighted welfare median
#' @param ppp_year numeric PPP year - 2017 (default) or 2011
#'
#' @return numeric value giving SPL for given
#' @export
#'
#' @examples
md_compute_spl <- function(
    welfare = NULL,
    weight = NULL,
    weighted_median_welfare = NULL,
    ppp_year = c(2017, 2011)
){
  # Input Checks
  stopifnot(ppp_year %in% c(2017, 2011))
  stopifnot(                                       # stop if not
    any(                                           #   either...
      c(
        all(!is.null(welfare), !is.null(weight)),  #   both welfare and weight not NULL
        !is.null(weighted_median_welfare)          #   weighted_median_welfare not NULL
      )
    )
  )

  # if weighted median is NULL then calculate it
  if(is.null(weighted_median_welfare)){

    weighted_median_welfare <- get_weighted_median(
      x = welfare,
      weight = weight
    )

  }

  threshold_rate <-  0.5

  if (ppp_year == 2011) {
    constant  <- 1
    min_level <- 1.9
  } else if (ppp_year == 2017) {
    constant  <- 1.15
    min_level <- 2.15
  }

  # Calculate SPL according to threshold rate
  spl <- constant + threshold_rate*weighted_median_ppp

  # Set minimum level if needed
  spl[spl < min_level] <- min_level

  return(spl)

}

