#' md_infer_poverty_line
#'
#' calculate percentiles corresponding to the specified  Shared of population
#' (percentages)
#'
#' @param welfare numeric: A vector of income or consumption values.
#' @param weight numeric: A vector of weights, optional.
#' @param popshare numeric: Share of population for which the corresponding
#' quantile is desired. Default .5 (i.e., weighted median)
#' @param na.rm logical: Set to TRUE to remove missing from computations
#' @param include logical:
#'
#' @return numeric
#' @export
#'
#' @examples
#' df <- md_ABC_2000_income
#' # median
#' md_infer_poverty_line(df$welfare, df$weight)
#' md_infer_poverty_line(df$welfare, df$weight, popshare = .2)
#' md_infer_poverty_line(df$welfare, df$weight, popshare = .6)
#'
md_infer_poverty_line <- function(welfare,
                                  weight = NULL,
                                  popshare = .5,
                                  na.rm = FALSE,
                                  include = FALSE) {

  # if weight is not provided
  if (is.null(weight)) {
    weight <- rep(1, length(welfare))
  }

  # make sure data is sorted properly
  o <- order(welfare)
  weight <- weight[o]
  welfare <- welfare[o]

  prob   <- cumsum(weight)/sum(weight, na.rm = na.rm)
  ps     <- which(abs(prob - popshare) == min(abs(prob - popshare), na.rm = na.rm))

  # Weighted mean with the next available value in order to
  # guarantee inclusion in poverty calculation

  if (include == TRUE) {
    pctile <- weighted.mean(c(welfare[ps], welfare[ps + 1]), c(weight[ps], weight[ps + 1]))
  } else {
    pctile <- mean(welfare[ps])
  }

  return(pctile)
}
