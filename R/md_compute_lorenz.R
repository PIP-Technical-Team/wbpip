#' Lorenz curve
#'
#' Compute the Lorenz curve for microdata.
#'
#' Given a vector of weights and welfare, this functions computes the
#' Lorenz curve.
#'
#' @param welfare numeric: A vector of income or consumption values.
#' @param weight numeric: A vector of weights.
#' @param nbins numeric: number of points on the Lorenz curve.
#'
#' @examples
#' wbpip:::md_compute_lorenz(welfare = 1:2000, weight = rep(1, 2000))
#' @return data.frame
#' @keywords internal
md_compute_lorenz <- function(welfare, weight, nbins = NULL) {

  nobs <- length(weight)
  if (is.null(nbins)) {
    # Define number of points on the Lorenz curve
    if (nobs > 1000) nbins <- 100 else nbins <- 20
  }

  # Set initial parameters
  cum_weight <- cumsum(weight)
  welfare_step <- sum(weight) / nbins
  # METHODOLOGY QUESTION: Should this hard coded 0.9999 be changed?
  # Not sure why it is here... Most likely to handle some edge case. I tested
  # the code without it, and it worked fine...
  levels <- welfare_step * 1:nbins * 0.999999999
  points <- vector("integer", nbins)
  j <- 1

  # Create points vector
  for (i in seq_len(nobs)) {
    while ((cum_weight[i] >= levels[j]) & (j <= nbins)) {
      points[j] <- i
      j <- j + 1
    }
  }

  # Create Lorenz curve vectors
  lorenz_welfare <- cumsum(welfare * weight) / sum(welfare * weight)
  lorenz_weight <- cumsum(weight) / sum(weight)

  # Select points on the Lorenz curve
  welfare <- welfare[points]
  lorenz_welfare <- lorenz_welfare[points]
  lorenz_weight <- lorenz_weight[points]

  return(data.frame(welfare,
                    lorenz_welfare,
                    lorenz_weight))

}

