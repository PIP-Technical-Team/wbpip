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
                              nbins  = if (length(welfare) > 1000) 100 else 20,
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

  p <- fcumsum(weight)/sum(weight)
  L <- fcumsum(weighted_welfare)/sum(weighted_welfare)

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
    uniq_fi <- funique(fi)
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


