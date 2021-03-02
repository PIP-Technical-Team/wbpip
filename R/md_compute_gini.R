#' Gini coefficient
#'
#' Compute the Gini coefficient for microdata.
#'
#' Given a vector of income or consumption values and their respective weights
#' `md_compute_gini()` computes the Gini coefficient for the distribution.
#'
#' @param welfare numeric: A vector of income or consumption values.
#' @param weight numeric: A vector of weights.
#'
#' @examples
#' wbpip:::md_compute_gini(welfare = 1:2000, weight = rep(1, 2000))
#'
#' @return numeric
#' @keywords internal
md_compute_gini <- function(welfare, weight) {

  N    <- collapse::fsum(weight)       # population size
  # Y    <- sum(y*w)     # total welfare

  cw   <- cumsum(weight)    # Cumulative weights
  # cy   <- cumsum(y*w)  # Cumulative welfare

  # sn   <-  weight/N         # share of population
  my   <- collapse::fmean(x = welfare, w = weight)

  i    <- (2*cw - weight + 1)/2
  t2   <- welfare*(N - i + 1)
  gini <- 1+(1/N) - (2/(my*N^2))* collapse::fsum(t2*weight)


  return(gini)
}









