
#' Compute quantiles share
#'
#' Compute quantiles for microdata.
#'
#' Calculate share of total welfare in each quantile from Lorenz curve
#'
#' @param welfare numeric: A vector of income or consumption values.
#' @param weight numeric: A vector of weights. Default is a vector of ones
#' @param n_quantile numeric: Number of quantiles for which share of total income
#' is desired. Default is 10.
#' @param lorenz numeric: Output from `md_compute_lorenz`
#'
#' @return list
#' @keywords internal
md_compute_quantiles_share <- function(welfare    = NULL,
                                       weight     = rep(1, length(welfare)),
                                       n_quantile = 10,
                                       lorenz     = NULL){
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Compute Lorenz if NULL  ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(lorenz) ||
      !n_quantile == nrow(lorenz)) {
    lorenz <- md_compute_lorenz(welfare = welfare,
                                weight  = weight,
                                nbins   = n_quantile)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Compute share with lorenz   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  share_quant <- diff(c(0, lorenz$lorenz_welfare))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  share_quant

}

#' Compute quantiles for microdata with lorenz function
#'
#' @param welfare numeric: A vector of income or consumption values.
#' @param weight numeric: A vector of weights. Default is a vector of ones,
#' @param n_quantiles numeric: Number of quantiles for which share of total income
#' is desired. Default is 10.
#' @param lorenz numeric: Output from `md_compute_lorenz`
#'
#' @return list
#' @export
#'
#' @examples
#' md_compute_quantiles(welfare = 1:2000, weight = rep(1, 2000))
#' @keywords internal
md_compute_quantiles <- function(welfare    = NULL,
                                 weight     = rep(1, length(welfare)),
                                 n_quantile = 10,
                                 lorenz     = NULL) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  estimate_lorenz <-
    if (is.null(lorenz)) {
      TRUE
    } else if (!n_quantile == nrow(lorenz)) {
      TRUE
    } else {
      FALSE
    }

  if (estimate_lorenz) {
    lorenz <- md_compute_lorenz(welfare = welfare,
                                weight  = weight,
                                nbins   = n_quantile)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  lorenz$welfare

}

#' Compute median for microdata
#'
#' @param welfare numeric: A vector of income or consumption values.
#' @param weight numeric: A vector of weights. Default is a vector of ones
#' @param lorenz numeric: Output from `md_compute_lorenz`
#'
#' @return numeric
#' @export
#'
#' @examples
#' md_compute_median(welfare = 1:2000, weight = rep(1, 2000))
#' @keywords internal
md_compute_median <- function(welfare,
                              weight = rep(1, length(welfare)),
                              lorenz = NULL) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(lorenz)) {
    lorenz <- md_compute_lorenz(
      welfare    = welfare,
      weight     = weight,
      nbins      = 10
    )
  }

  n  <- fnrow(lorenz)

  if (n %% 2 == 0) {
    median <- lorenz$welfare[n/2]
  } else {
    w1 <- lorenz$lorenz_weight[(n - 1)/2]
    w2 <- lorenz$lorenz_weight[(n + 1)/2]
    if (abs(0.5 - w1) < abs(0.5 - w2)) {
      median <- lorenz$welfare[(n - 1)/2]
    } else {
      median <- lorenz$welfare[(n + 1)/2]
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  median

}

#' Compute quantiles for microdata with collapse
#'
#' @param welfare numeric: A vector of income or consumption values.
#' @param weight numeric: A vector of weights. Default is a vector of ones,
#' @param n_quantiles numeric: Number of quantiles for which share of total income
#' is desired. Default is 10.
#'
#' @return list
#' @keywords internal
md_compute_quantiles_c <- function(welfare,
                                    weight = rep(1, length(welfare)),
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
  quantiles   <- fquantile(welfare, probs = probs, w = weight, type = 7)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(quantiles = quantiles)

}



#' Welfare share by quantile in micro data
#'
#' `md_welfare_share_at` returns the share of welfare held by an specified
#' share of the population. You can select the number of quantiles (10 be default).
#' This function makes use of `md_compute_lorenz`.
#'
#' @inheritParams md_compute_quantiles_share
#'
#' @return list with vector of share of welfare by quantiles
#' @export
#'
#' @examples
#' md_compute_cumulative_share(welfare = md_GHI_2000_consumption$welfare,
#'                             weight = md_GHI_2000_consumption$weight)
md_compute_cumulative_share <- function(
    welfare     = NULL,
    weight      = rep(1, length = length(welfare)),
    n_quantile  = 10,
    lorenz      = NULL){

  # ____________________________________________________________________
  # Calculations --------------------------------------------------
  estimate_lorenz <-
    if (is.null(lorenz)) {
      TRUE
    } else if (!n_quantile == nrow(lorenz)) {
      TRUE
    } else {
      FALSE
    }

  if (estimate_lorenz) {
    lorenz <- md_compute_lorenz(welfare = welfare,
                                weight  = weight,
                                nbins   = n_quantile)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  lorenz$lorenz_welfare

}



