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
#'
#' @return data.frame
#' @keywords internal
md_compute_lorenz <- function(welfare, weight, nbins = NULL) {

  nobs <- length(weight)
  if (is.null(nbins)) {
    # Define number of points on the Lorenz curve
    if (nobs > 1000) nbins <- 100 else nbins <- 20
  }

  # Placeholder for Lorenz curve
  welfare_col    <- vector(mode = "numeric", length = nbins)
  lorenz_welfare <- vector(mode = "numeric", length = nbins)
  lorenz_weight  <- vector(mode = "numeric", length = nbins)

  # Compute Lorenz curve
  weighted_welfare     <- weight * welfare
  sum_weighted_welfare <- sum(weighted_welfare)
  sum_weights          <- sum(weight)
  welfare_step         <- sum_weights / nbins
  next_level           <- welfare_step
  cum_weight           <- 0 # Placeholder for cumulative weight
  cum_welfare          <- 0 # Placeholder for cumulative welfare
  j                    <- 1


  for (i in seq_len(nobs)) {
    cum_weight  <- cum_weight + weight[i] # Cumulative weight
    cum_welfare <- cum_welfare + weighted_welfare[i] # Cumulative income

    while ((cum_weight >= next_level) & (j <= nbins)) {
      welfare_col[j]    <- welfare[i]
      lorenz_welfare[j] <- cum_welfare / sum_weighted_welfare # Normalize cum_welfare
      lorenz_weight[j]  <- cum_weight / sum_weights           # Normalize cum_weight

      j = j + 1
      # METHODOLOGY QUESTION: Should this hard coded 0.9999 be changed?
      # Not sure why it is here... Most likely to handle some edge case. I tested
      # the code without it, and it worked fine...
      if (j <= nbins) {next_level <- welfare_step * j * 0.999999999}
    }
  }

  lorenz <- data.frame(
    welfare        = welfare_col,
    lorenz_welfare = lorenz_welfare,
    lorenz_weight  = lorenz_weight
  )

  return(lorenz)

}
