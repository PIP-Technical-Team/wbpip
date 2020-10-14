#' md_compute_lorenz
#' Given a vector of weights and welfare, this functions computes the
#' Lorenz curve
#' @param welfare numeric: vector of welfare measures
#' @param weight numeric: vector of weights
#'
#' @return data.frame
#'
#' #' @examples
#' md_compute_lorenz(welfare = 1:2000, weight = 1:2000)
#'
#' data("md_ABC_2000_income")
#' md_compute_lorenz(md_ABC_2000_income$welfare, md_ABC_2000_income$weight)
#'
#' @export
#'

md_compute_lorenz <- function(welfare,
                              weight = NULL,
                              nbins  = NULL) {

  # Set all weights to 1 if none are supplied
  if (is.null(weight)) {
    weight <- rep(1, length(welfare))
  }

  nobs <- length(weight)

  if (is.null(nbins)) {
    nbins <- ifelse(nobs > 1000, 100, 20) # Define number of points on the Lorenz curve
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
