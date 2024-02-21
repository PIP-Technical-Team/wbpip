#' Compute quantiles (old version)
#'
#' Compute quantiles for microdata.
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
#' @param tolerance numeric: Tolerance parameter for `lorenzw >= nextQ` check.
#'
#' @examples
#' lz <- wbpip:::old_md_compute_lorenz(welfare = 1:2000, weight = rep(1, 2000))
#' wbpip:::old_md_compute_quantiles(
#'   lwelfare = lz$lorenz_welfare,
#'   lweight = lz$lorenz_weight,
#'   percentile = lz$welfare,
#'   n_quantile = 10
#' )
#' @return list
#' @keywords internal
old_md_compute_quantiles <- function(lwelfare,
                                 lweight,
                                 percentile,
                                 n_quantile = 10,
                                 tolerance = 1e-06) {


  #--------- Consistency ---------

  n_lorenz <- length(lwelfare)
  assertthat::assert_that(n_quantile < n_lorenz,
    msg = "The number of requested quantiles is superior to the number of points on the Lorenz curve"
  )


  #--------- Make sure data is sorted properly ---------
  # I assume the three vectors are of the same length

  or <- order(percentile)
  lwelfare <- lwelfare[or]
  lweight <- lweight[or]
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
    yi <- percentile[i] # Percentile of income
    lorenzw <- lweight[i] # Cumulative share of population
    lorenzy <- lwelfare[i] # Cumulative share of income / consumption

    if (lorenzw > nextQ | assertthat::are_equal(lorenzw, nextQ, tolerance = tolerance)) {
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

#' Compute quantiles share
#'
#' Compute quantiles for microdata.
#'
#' Calculate share of total welfare in each quantile from Lorenz curve and
#' the its corresponding percentile. That is, it requires a vector with the
#' cumulative share of the population (`lweight`), a vector with cumulative
#' share of welfare (`lwelfare`), and a vector the corresponding monetary value
#' of each percentile (`percentile`).
#'
#' @param welfare numeric: A vector of income or consumption values.
#' @param weight numeric: A vector of weights. Default is a vector of ones,
#' @param n_quantile numeric: Number of quantiles for which share of total income
#' is desired. It can't be larger that the total number of percentiles in the
#' Lorenz curve provided by the user.  default is 10.
#'
#' @examples
#' md_compute_quantiles_share(welfare = 1:2000, weight = rep(1, 2000))
#'
#' @return list
#' @keywords internal
md_compute_quantiles_share <- function(welfare,
                                        weight,
                                        n_quantile = 10){
  # # deal with NAs -----
  # if (anyNA(welfare)) {
  #   ina      <- !is.na(welfare)
  #   weight   <- weight[ina]
  #   welfare  <- as.numeric(welfare)[ina]
  # }
  #
  # if (anyNA(weight)) {
  #   ina      <- !is.na(weight)
  #   weight   <- weight[ina]
  #   welfare  <- as.numeric(welfare)[ina]
  # }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Compute Lorenz   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  lz <- md_compute_lorenz(welfare, weight, nbins = n_quantile)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Compute share with quantile function and collapse   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # qnt <- md_compute_quantiles_c(welfare, weight, n_quantile)
  #
  # cum_share <- vector("numeric",n_quantile)
  #
  # for (i in seq_len(n_quantile)){
  #   qnt_i <- qnt[i]
  #   cum_share[i] <- fsum(welfare[welfare<=qnt_i],
  #                          w = weight[welfare<=qnt_i]) / fsum(welfare, w = weight)
  # }
  #
  # share_quant <- diff(c(0,cum_share))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Compute share with lorenz   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  share_quant <- diff(c(0,lz$lorenz_welfare))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(share_quant = share_quant)

}

#' Compute quantiles for microdata with lorenz function
#'
#' @param welfare numeric: A vector of income or consumption values.
#' @param weight numeric: A vector of weights. Default is a vector of ones,
#' @param n_quantiles numeric: Number of quantiles for which share of total income
#' is desired. It can't be larger that the total number of percentiles in the
#' Lorenz curve provided by the user.  default is 10.
#'
#' @return list
#' @export
#'
#' @examples
#' md_compute_quantiles(welfare = 1:2000, weight = rep(1, 2000))
#' @keywords internal
md_compute_quantiles <- function(welfare,
                                  weight,
                                  n_quantile = 10) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  lz <- md_compute_lorenz(welfare, weight, nbins = n_quantile)
  quantiles <- lz$welfare

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(quantiles = quantiles)

}

#' Compute median for microdata
#'
#' @param welfare numeric: A vector of income or consumption values.
#' @param weight numeric: A vector of weights. Default is a vector of ones,
#'
#' @return numeric
#' @export
#'
#' @examples
#' md_compute_quantiles(welfare = 1:2000, weight = rep(1, 2000))
#' @keywords internal
md_compute_median <- function(welfare,
                                 weight) {
  # deal with NAs -----
  if (anyNA(welfare)) {
    ina      <- !is.na(welfare)
    weight   <- weight[ina]
    welfare  <- as.numeric(welfare)[ina]
  }

  if (anyNA(weight)) {
    ina      <- !is.na(weight)
    weight   <- weight[ina]
    welfare  <- as.numeric(welfare)[ina]
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  median <- collapse::fmedian(welfare, w = weight)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(median = median)

}

#' Compute quantiles for microdata with collapse
#'
#' @param welfare numeric: A vector of income or consumption values.
#' @param weight numeric: A vector of weights. Default is a vector of ones,
#' @param n_quantiles numeric: Number of quantiles for which share of total income
#' is desired. It can't be larger that the total number of percentiles in the
#' Lorenz curve provided by the user.  default is 10.
#'
#' @return list
#' @export
#'
#' @examples
#' md_compute_quantiles_c(welfare = 1:2000, weight = rep(1, 2000))
#' @keywords internal
md_compute_quantiles_c <- function(welfare,
                                    weight,
                                    n_quantile = 10) {
  # deal with NAs -----
  if (anyNA(welfare)) {
    ina      <- !is.na(welfare)
    weight   <- weight[ina]
    welfare  <- as.numeric(welfare)[ina]
  }

  if (anyNA(weight)) {
    ina      <- !is.na(weight)
    weight   <- weight[ina]
    welfare  <- as.numeric(welfare)[ina]
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  bins_groups <- 1:n_quantile
  probs       <- bins_groups/n_quantile
  quantiles <- collapse::fquantile(welfare, probs = probs, w = weight, type=7)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(quantiles = quantiles)

}
