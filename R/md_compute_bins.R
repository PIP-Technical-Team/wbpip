#' md_compute_bins
#'
#' Compute percentiles (bins) accumulating by population rather than by welfare.
#' It differs from the common way to calculate percentiles because the categories
#' are defined by the ideal size of the quantile rather than by the cutpoints.
#' This is way we call the function `bins` rather than quantiles because it
#' actually divides the population in equal bins sorted by the welfare aggregate
#' rather than calculate proper percentiles. This function however,  yields less
#' unequally sized categories when [1] the cutpoint value is frequent, [2] when
#' using weights or [3] when the number of observations in the dataset is not a
#' product of the number of quantiles. Keep in mind that it may not work properly
#' on small datasets or if calculated for small groups. If the number of
#' observations in the dataset or group is small, tabulate afterwards to
#' check the results. you may use
#' `tp <- df[, sum(weight)]; df[, round((sum(weight)/(tp))*100, digits = 2) , by = bins]`.
#' @param welfare numeric: vector of welfare measures
#' @param weight numeric: vector of weights
#'
#' @return dataframe in data.table format
#' @export
#' @import data.table
#'
#' @examples
#' data("md_ABC_2010_income")
#' df <- md_compute_bins(md_ABC_2010_income$welfare, md_ABC_2000_income$weight)
md_compute_bins <- function(welfare,
                            weight = NULL,
                            nbins  = 100,
                            na.rm  = FALSE) {

  # Set all weights to 1 if none are supplied
  if (is.null(weight)) {
    weight <- rep(1, length(welfare))
  }

  #--------- Create data.table for fast calculations ---------
  dt <- data.table::data.table(welfare = welfare,
                               weight  = weight)
  data.table::setorder(dt, welfare)

  total_pop <- dt[,sum(weight, na.rm = na.rm)]  # total population

  #--------- Calculations ---------

  dt[,
     c("cum_pop", "cum_prop_pop", "bins") := {

       cum_pop      = cumsum(weight)     # cumulative population
       cum_prop_pop = cum_pop/total_pop  # cumulative proportion of population
       bins         = ceiling(cum_prop_pop*nbins) # Bins

       list(cum_pop, cum_prop_pop, bins)
     }
     ]

  return(dt)
}
