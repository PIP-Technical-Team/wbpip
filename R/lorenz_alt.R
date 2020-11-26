#' @import data.table
NULL

# Add global variables to avoid NSE notes in R CMD check
if (getRversion() >= '2.15.1')
  utils::globalVariables(
    c('bins', '.')
  )

#' lorenz_alt
#'
#' Given a vector of weights and welfare, this functions computes the
#' Lorenz curve and several other related indicators.
#'
#' @param welfare numeric: A vector of income or consumption values.
#' @param weight numeric: A vector of weights.
#' @param nbins numeric: number of points on the Lorenz curve.
#' @param na.rm logical: Set to TRUE to remove missing.
#'
#' @examples
#' wbpip:::lorenz_alt(welfare = 1:2000, weight = rep(1, 2000))
#'
#' @return data.table
#' @keywords internal
lorenz_alt <- function(welfare, weight,
                       nbins  = NULL,
                       na.rm  = FALSE) {

  nobs <- length(weight)

  if (is.null(nbins)) {
    nbins <- ifelse(nobs > 1000, 100, 20) # Define number of points on the Lorenz curve
  }

  #--------- Create data.table for fast calculations ---------
  dt <- data.table::data.table(welfare = welfare,
                               weight  = weight)
  data.table::setorder(dt, welfare)  # sort and index by reference

  #--------- constants ---------
  total_pop <- dt[, sum(weight, na.rm = na.rm)]  # total population
  mean_welf <- dt[, stats::weighted.mean(welfare, weight, na.rm = na.rm)]

  #--------- variables ---------

  dt[, bins  := md_compute_bins(welfare, weight)] # calculate quantiles

  # collapse by bin
  dtc <- dt[,
            .(welfare = sum(welfare*weight, na.rm = na.rm), # total welfare per bin
              weight  = sum(weight, na.rm = na.rm),         # total population per bin
              maxw    = max(welfare, na.rm = na.rm)         # threshold by bin
            ),
            by = bins
            ]

  #--------- calculation of variables ---------
  dtc[,
      c("cum_pop",
        "prop_pop",
        "cum_prop_pop",
        "cum_welfare",
        "prop_welfare",
        "lorenz",
        "gn_lorenz",
        "cum_mean_welfare"
      )
      := {

        cum_pop       <- cumsum(weight)       # Cumulative population
        prop_pop      <- cum_pop / total_pop  # share of population
        cum_prop_pop  <- cumsum(prop_pop)     # Cumulative share of population

        cum_welfare   <- cumsum(welfare)     # Cumulative welfare
        total_welfare <- sum(welfare, na.rm = na.rm)  # total welfare
        prop_welfare  <- welfare / total_welfare     # share of welfare
        lorenz        <- cumsum(prop_welfare)        # cumulative share of welfare "Lorenz"
        gn_lorenz     <- lorenz * mean_welf          # generalized Lorenx curve

        cum_mean_welfare <- cum_welfare / cum_pop       # cumulative mean

        list(cum_pop,
             prop_pop,
             cum_prop_pop,
             cum_welfare,
             prop_welfare,
             lorenz,
             gn_lorenz,
             cum_mean_welfare)
      }]

  #--------- Labels ---------

  attr(dtc$cum_pop ,          "label") <- "Cumulative population"
  attr(dtc$prop_pop ,         "label") <- "share of population"
  attr(dtc$cum_prop_pop ,     "label") <- "Cumulative share of population"
  attr(dtc$cum_welfare ,      "label") <- "Cumulative welfare"
  attr(dtc$prop_welfare ,     "label") <- "share of welfare"
  attr(dtc$lorenz ,           "label") <- "Lorenz (cumulative share of welfare)"
  attr(dtc$gn_lorenz ,        "label") <- "generalized Lorenz curve"
  attr(dtc$cum_mean_welfare , "label") <- "cumulative mean"
  attr(dtc$maxw ,             "label") <- "Bin threshold"

  return(dtc)

}

