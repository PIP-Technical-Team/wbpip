#' Lorenz curve
#'
#' Compute the Lorenz curve for microdata.
#'
#' Given a vector of weights and welfare, this functions computes the Lorenz
#' curve.
#'
#' @param welfare numeric: A vector of income or consumption values.
#' @param weight numeric: A vector of weights. Default is a vector of ones,
#'   `rep(1, length(welfare))`.
#' @param nbins numeric: number of points on the Lorenz curve. if `NULL` the
#'   returning  Lorenz curve would be the  length of the original welfare vector
#'   minus the number of `NAs` of different observations in  `welfare` and
#'   `weight`. Default is `100` for `length(welfare) > 1000` and `20` otherwise.
#' @param force_nbins logical; Force the creation of exact nbins even there is
#'   no actual data that falls in the corresponding interval. This implies that
#'   some observations will be repeated.
#'
#' @examples
#' md_compute_lorenz(welfare = md_ABC_2010_income$welfare,
#' weight = md_ABC_2010_income$weight)
#' @return data.frame
#' @export
md_compute_lorenz <- function(welfare,
                              weight = rep(1, length(welfare)),
                              nbins = if (length(welfare) > 1000) 100 else 20,
                              force_nbins = TRUE) {


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

  # Sort data ------

  if (is.unsorted(welfare)) {
    o       <- order(welfare) # this is faster than collapse::radixorder
    welfare <- welfare[o]
    weight  <- weight[o]
  }

  # Compute Lorenz curve  -----
  weighted_welfare     <- weight * welfare

  p <- collapse::fcumsum(weight)/collapse::fsum(weight)
  L <- collapse::fcumsum(weighted_welfare)/collapse::fsum(weighted_welfare)



  # get nbins  ----
  if (!is.null(nbins)) {
    # probs   <- seq(0,1, 1/nbins)

    bins_groups <- 1:nbins
    probs       <- bins_groups/nbins
    bins_groups <- c(0, bins_groups) # zero

    # getting indexes
    rounding <- 10
    fi       <- findInterval(round(p, rounding), round(probs, rounding))
    di       <- diff(c(0, fi)) # [1] See notes.
    rp       <- which(di >= 1) # [2] See notes.

    # in case there are empty bins.
    uniq_fi <- collapse::funique(fi)
    if (!collapse::all_obj_equal(uniq_fi, bins_groups) &
        force_nbins ) {
      ind  <- rep(rp, di[rp]) # [3] See notes.
    } else {
      ind <- rp
    }

    # extract the values from the original vectors.
    p       <- p[ind]
    L       <- L[ind]
    welfare <- welfare[ind]
  }

  # return ----------

  lorenz <- data.frame(
    welfare        = welfare,
    lorenz_welfare = L,
    lorenz_weight  = p
  )
  return(lorenz)

}

# Notes on the code above
# [1] find differences in intervals. If all intervals are found, differences
# should be 1s. If one or more subsequent intervals are not found, the
# difference will be higher than 1. I add the zero(0) to account in case the
# first interval is not found.
# [2] Find which observations account for the change of interval. Since we added
# a zero in the previous step, there is no need to sum 1.
# [3] Repeat index according to the difference. e.g., If no interval is found,
# the difference will 2 and those the index should be counted twice in order
# to end up with nbins.


#' Lorenz curve (old methodoly)
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' This functions has been deprecated because it was inefficient. The original
#' name of the function was [md_compute_lorenz()] but it has been changed to
#' `old_md_compute_lorenz()` because the new version requires the original name
#' for compatibility with other PIP products. This function is available only
#' for replicability  and testing purposes.
#'
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
#' wbpip:::old_md_compute_lorenz(welfare = 1:2000, weight = rep(1, 2000))
#' @return data.frame
#' @keywords internal
old_md_compute_lorenz <- function(welfare, weight, nbins = NULL) {
  nobs <- length(weight)
  if (is.null(nbins)) {
    # Define number of points on the Lorenz curve
    if (nobs > 1000) nbins <- 100 else nbins <- 20
  }

  # Placeholder for Lorenz curve
  welfare_col <- vector(mode = "numeric", length = nbins)
  lorenz_welfare <- vector(mode = "numeric", length = nbins)
  lorenz_weight <- vector(mode = "numeric", length = nbins)

  # Compute Lorenz curve
  weighted_welfare <- weight * welfare
  sum_weighted_welfare <- sum(weighted_welfare)
  sum_weights <- sum(weight)
  welfare_step <- sum_weights / nbins
  next_level <- welfare_step
  cum_weight <- 0 # Placeholder for cumulative weight
  cum_welfare <- 0 # Placeholder for cumulative welfare
  j <- 1


  for (i in seq_len(nobs)) {
    cum_weight <- cum_weight + weight[i] # Cumulative weight
    cum_welfare <- cum_welfare + weighted_welfare[i] # Cumulative income

    while ((cum_weight >= next_level) & (j <= nbins)) {
      welfare_col[j] <- welfare[i]
      lorenz_welfare[j] <- cum_welfare / sum_weighted_welfare # Normalize cum_welfare
      lorenz_weight[j] <- cum_weight / sum_weights # Normalize cum_weight

      j <- j + 1
      # METHODOLOGY QUESTION: Should this hard coded 0.9999 be changed?
      # Not sure why it is here... Most likely to handle some edge case. I tested
      # the code without it, and it worked fine...
      if (j <= nbins) {
        next_level <- welfare_step * j * 0.999999999
      }
    }
  }

  lorenz <- data.frame(
    welfare        = welfare_col,
    lorenz_welfare = lorenz_welfare,
    lorenz_weight  = lorenz_weight
  )

  return(lorenz)
}
