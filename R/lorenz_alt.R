#' lorenz
#'
#' Given a vector of weights and welfare, this functions computes the
#' Lorenz curve and selveral other related indicators.
#' @param welfare numeric: vector of welfare measures
#' @param weight numeric: vector of weights
#' @param nbins numeric: number of points on the Lorenz curve
#' @param na.rm logical: Set to TRUE to remove missing
#'
#' @return data.frame
#' @export
#' @import data.table
#'
#' @examples
#' data("md_ABC_2010_income")
#' df <- md_ABC_2010_income
#' lz <- lorenz_alt(df$welfare, df$weight)[]
lorenz_alt <- function(welfare,
                    weight = NULL,
                    nbins  = NULL,
                    na.rm  = FALSE) {

  # Set all weights to 1 if none are supplied
  if (is.null(weight)) {
    weight <- rep(1, length(welfare))
  }

  nobs <- length(weight)

  if (is.null(nbins)) {
    nbins <- ifelse(nobs > 1000, 100, 20) # Define number of points on the Lorenz curve
  }

  #--------- Create data.table for fast calculations ---------
  dt <- data.table::data.table(welfare = welfare,
                               weight  = weight)
  data.table::setorder(dt, welfare)  # sort and indez by reference


  #--------- constants ---------
  total_pop <- dt[, sum(weight, na.rm = na.rm)]  # total population
  mean_welf <- dt[, weighted.mean(welfare, weight, na.rm = na.rm)]

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

