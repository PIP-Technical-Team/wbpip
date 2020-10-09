#' calculate percentiles corresponding to the specified  Shared of population
#' (percentages)
#'
#' @param welfare numeric: A vector of income or consumption values.
#' @param weight numeric: A vector of weights, optional.
#' @param popshare numeric: Share of population for which the corresponding
#' quantile is desired. Default .5 (i.e., weighted median)
#' @param sum_weights numeric: Total population. Default is `sum(weight)`. If
#' provided `weight` will be adjusted accordingly,
#' `weight <- weight * (sum_weights/sum(weight))`
#'
#' @return welfare value (quantile) that corresponds
#' @export
#'
#' @examples
md_infer_poverty_line <- function(welfare,
                                  weight = NULL,
                                  popshare = .5,
                                  sum_weights = NULL,
                                  alternative = FALSE,
                                  na.rm = FALSE) {

  # if weight is not provided
  if (is.null(weight)) {
    weight <- rep(1, length(welfare))
  }

  # make sure data is sorted properly
  o       <- order(welfare)
  weight  <- weight[o]
  welfare <- welfare[o]

  # Treatment of Total population
  if (is.null(sum_weights)) {

    sum_weights <- sum(weight, na.rm = na.rm)

  } else {

    weight <- weight * (sum_weights/sum(weight, na.rm = na.rm))

  }

  headcount <- 0
  # pr.h    <- popshare # headcount defined as popshare
  wtile     <- popshare * sum_weights # Number of people below PL in survey sample
  lastY     <- 0
  pl        <- NA

  for (i in seq_along(welfare)) {

    wi <- weight[i]
    yi <- welfare[i]

    if (sum(headcount, wi, na.rm = na.rm) < wtile) {

      headcount <- sum(headcount, wi, na.rm = na.rm)
      lastY     <- yi

    } else {

      pl <- lastY + (yi - lastY) * (wtile - headcount) / wi # infer poverty line from h
      break

    }
  }

  return(pl)

}
