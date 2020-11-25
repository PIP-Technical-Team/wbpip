#' Infer poverty line
#'
#' Infer poverty line for microdata.
#'
#' This function calculate percentiles corresponding to the specified share of
#' population (in percentages).
#'
#' @param welfare numeric: A vector of income or consumption values.
#' @param weight numeric: A vector of weights.
#' @param popshare numeric: Share of population for which the corresponding.
#' quantile is desired. Default .5 (i.e., weighted median).
#' @param na.rm logical: Set to TRUE to remove missing from computations.
#' @param include logical: **TO BE DOCUMENTED**.
#'
#' @examples
#' wbpip:::md_infer_poverty_line(1:2000, weight = rep(1, 2000))
#' wbpip:::md_infer_poverty_line(1:2000, weight = rep(1, 2000),
#'   popshare = .2)
#' wbpip:::md_infer_poverty_line(1:2000, weight = rep(1, 2000),
#'   popshare = .6)
#'
#' @return numeric
#' @keywords internal
md_infer_poverty_line <- function(welfare,
                                  weight,
                                  popshare = .5,
                                  na.rm = FALSE,
                                  include = FALSE) {

  prob   <- cumsum(weight) / sum(weight, na.rm = na.rm)
  ps     <- which(abs(prob - popshare) == min(abs(prob - popshare), na.rm = na.rm))

  # Weighted mean with the next available value in order to
  # guarantee inclusion in poverty calculation

  if (include == TRUE) {
    pctile <- stats::weighted.mean(c(welfare[ps], welfare[ps + 1]),
                                   c(weight[ps], weight[ps + 1]))
  } else {
    pctile <- mean(welfare[ps])
  }

  return(pctile)
}
