#' Compute Poverty Statistics
#'
#' Compute the poverty headcount, poverty gap, poverty severity and
#' the watts index for a micro data set.
#'
#' Given a vector of consumption or income values and their respective weights
#' `md_compute_poverty_stats()` computes poverty headcount, poverty gap,
#' poverty severity and the watts index.
#'
#' Negative values are dropped from computations.
#'
#' @param welfare numeric: A vector of income or consumption values
#' @param povline numeric: A poverty line/threshold
#' @param weight numeric: A vector of weights, optional, a vector of 1s if not specified.
#'
#'
#' @return List object containing poverty headcount, poverty gap, poverty severity and the watts index
#' @export
#'
#' @examples
#' # simple example (no weights)
#' md_compute_poverty_stats(welfare = 1:100, povline = 10)
#'
#' # with weights
#' md_compute_poverty_stats(welfare = 1:100, povline = 10, weight = 1:100)
#'
#' # using a micro data set
#' data("md_ABC_2000_income")
#' md_compute_poverty_stats(md_ABC_2000_income$welfare, 2500000, md_ABC_2000_income$weight)
#'
#'
md_compute_poverty_stats <- function(welfare, povline, weight = NULL) {

  ## set all weights to 1 if none are supplied
  if (is.null(weight) == TRUE) {
    weight <- rep(1, length(welfare))
  }

  ## make sure the data is sorted
  ordy    <- order(welfare)   # order of welfare
  welfare <- welfare[ordy]    #order weight
  weight  <- weight[ordy]     # order welfare

  headcount <- 0
  gap <- 0
  severity <- 0
  watt8 <- 0

  for (i in seq_along(welfare)) {

    weight_i <- weight[i]
    welfare_i <- welfare[i]

    if (welfare_i <= povline) {

      headcount <- sum(headcount, weight_i)
      gap_i <- 1 - welfare_i / povline
      gap <- sum(gap, weight_i * gap_i)
      severity <- sum(severity, weight_i * gap_i ^ 2)
      if (welfare_i > 0) { # Is this check needed no negative welfare value should make it to the application
        watt8 <- sum(watt8, weight_i * log(povline / welfare_i))
      }

    }
  }

  #compute the values for the return
  gap <- gap / sum(weight)
  severity <- severity / sum(weight)
  watt8 <- watt8 / sum(weight)

  return(list(
    headcount = headcount,
    poverty_gap = gap,
    poverty_severity = severity,
    watts = watt8
  ))
}
