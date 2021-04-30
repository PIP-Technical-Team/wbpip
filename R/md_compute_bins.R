#' Compute bins
#'
#' Compute bins for microdata.
#'
#' Compute percentiles (bins) accumulating by population rather than by welfare.
#' It differs from the common way to calculate percentiles because the
#' categories are defined by the ideal size of the quantile rather than by the
#' cutpoints.
#'
#' This is why the function is called `bins` rather than quantiles because it
#' actually divides the population in equal bins sorted by the welfare aggregate
#' rather than calculate proper percentiles.
#'
#' This function however, yields less unequally sized categories when 1) the
#' cutpoint value is frequent, 2) when using weights high variance or 3) when
#' the number of observations in the dataset is not a product of the number of
#' quantiles. Keep in mind that it may not work properly on small datasets or if
#' calculated for small groups. If the number of observations in the dataset or
#' group is small, tabulate afterwards to check the results. you may use `tp <-
#' df[, sum(weight)]; df[, round((sum(weight)/(tp))*100, digits = 2) , by =
#' bins]`.
#'
#' @param welfare numeric: A vector of income or consumption values.
#' @param weight numeric: A vector of weights.
#' @param nbins  numeric: Number of bins.
#' @param na.rm  logical: If TRUE it will exclude all NA in calculations
#' @param output character: It has two varieties. 1) it could be a vector of
#'   variables to retain after calculations (variables available are "welfare",
#'   "weight", "cum_pop", "cum_prop_pop", and "bins"). 2) It could be a one of
#'   two key words, "simple" or "full". `output = "simple"` is equivalent to
#'   `output = "bins"` (which is the default). `output = "full"` if equivalent
#'   to a vector with all the variables available, `output = c("welfare",
#'   "weight", "cum_pop", "cum_prop_pop","bins")`
#'
#' @examples
#' wbpip:::md_compute_bins(welfare = 1:2000, weight = rep(1, 2000))
#' wbpip:::md_compute_bins(welfare = 1:2000, weight = rep(1, 2000),
#'  output = 'full')
#' wbpip:::md_compute_bins(welfare = 1:2000, weight = rep(1, 2000),
#'   output = c('cum_pop', 'cum_prop_pop'))
#'
#' @return data.table
#' @keywords internal
md_compute_bins <- function(welfare, weight,
                            nbins  = 100,
                            na.rm  = FALSE,
                            output = "simple") {

  #--------- Create data.table for fast calculations ---------
  dt <- data.table::data.table(welfare = welfare,
                               weight  = weight)
  data.table::setorder(dt, welfare)

  # total population
  total_pop <- collapse::fsum(x = dt$weight, na.rm = na.rm)

  #--------- Calculations ---------

  dt[,
     c("cum_pop", "cum_prop_pop", "bins") := {

       cum_pop      = cumsum(weight)     # cumulative population
       cum_prop_pop = cum_pop/total_pop  # cumulative proportion of population
       bins         = ceiling(cum_prop_pop*nbins) # Bins

       list(cum_pop, cum_prop_pop, bins)
     }
     ]

  #--------- labels ---------
  attr(dt$cum_pop,      "label")  <- "cumulative population"
  attr(dt$cum_prop_pop, "label")  <- "cumulative proportion of population"
  attr(dt$bins,          "label") <- "Quantiles"

  if ("simple" %in% output) {

    return(dt[, "bins"])

  } else if ("full" %in% output) {

    return(dt)

  } else {

    return(dt[, ..output])

  }
}
