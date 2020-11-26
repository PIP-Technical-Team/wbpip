#' md_compute_quantiles
#'
#' Calculate share of total welfare in each quantile from Lorenz curve and
#' the its corresponding percentile. That is, it requires a vector with the
#' cumulative share of the population (`lweight`), a vector with cumulative
#' share of welfare (`lwelfare`), and a vector the corresponding monetary value
#' of each percentile (`percentile`).
#'
#' @param lwelfare numeric: cumulative share of welfare.
#' @param lweight  numeric: cumulative share of population.
#' @param n_quantile numeric: Number of quantiles for which share of total income
#' is desired. It can't be larger that the total number of percentiles in the
#' Lorenz curve provided by the user.  default is 10.
#' @param percentile numeric: Monetary value each percentile.
#'
#' @return list
#' @export
#'
#' @examples
#' fpath  <- system.file("testdata", "lorenz.csv", package="wbpip")
#' lz     <- read.csv(fpath,
#'                    col.names = c("y", "lorenzW", "lorenzY"))
#' md_compute_quantiles(lwelfare   = lz$lorenzY,
#'                      lweight    = lz$lorenzW,
#'                      percentile = lz$y)
md_compute_quantiles <- function(lwelfare,
                                 lweight,
                                 percentile,
                                 n_quantile = 10) {


  #--------- Consistency ---------

  n_lorenz <- length(lwelfare)
  assertthat::assert_that(n_quantile < n_lorenz,
                          msg = "The number of requested quantiles is superior to the number of points on the Lorenz curve")


  #--------- Make sure data is sorted properly ---------
  # I assume the three vectors are of the same length

  or         <- order(percentile)
  lwelfare   <- lwelfare[or]
  lweight    <- lweight[or]
  percentile <- percentile[or]

  #--------- Initial parameters ---------
  # lastW = lastY = lastQ <- 0
  lastW <- 0
  lastY <- 0
  lastQ <- 0
  nextQ <- 1 / n_quantile
  quantiles <- rep_len(0, n_quantile)
  j <- 1
  step <- 1 / n_quantile

  #--------- Calculations ---------

  for (i in seq_len(n_lorenz)) {

    yi      <- percentile[i] # Percentile of income
    lorenzw <- lweight[i]    # Cumulative share of population
    lorenzy <- lwelfare[i]   # Cumulative share of income / consumption

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
