md_compute_quantiles <- function(lorenz, n_quantile = 10) {

  n_lorenz <- nrow(lorenz)

  assertthat::assert_that(n_quantile < n_lorenz,
                          msg = "The number of requested quantiles is superior to the number of points on the Lorenz curve")

  # lastW = lastY = lastQ <- 0
  lastW <- 0
  lastY <- 0
  lastQ <- 0
  nextQ <- 1 / n_quantile
  quantiles <- rep_len(0, n_quantile)
  j <- 1
  step <- 1 / n_quantile

  for (i in seq_len(n_lorenz)) {

    yi <- lorenz[['y']][i] # Percentile of income
    lorenzw <- lorenz[['lorenzW']][i] # Cumulative share of population
    lorenzy <- lorenz[['lorenzY']][i] # Cumulative share of income / consumption

    if (lorenzw >= nextQ) {
      if (nextQ == 0.5) {
        median <- yi
      }
      QY <- (nextQ - lastW) / (lorenzw - lastW) * (lorenzy - lastY) # interpolate the value of QY
      quantiles[j] <- sum(lastY, QY) - lastQ # All values are cumulative. lastQ needs to be removed to avoid double counting.
      lastQ <- sum(lastQ, quantiles[j])
      j <- sum(j, 1)
      nextQ <- sum(nextQ, step)
    }

    lastW <- lorenzw
    lastY <- lorenzy
  }

  return(list(quantiles = quantiles, median = median))
}
