#' Compute SPL
#'
#' Compute the societal poverty line based on the median daily consumption in US dollars
#'
#' @param welfare numeric: A vector of consumption or income values
#' @param weight numeric: A vector of weights corresponding for each welfare value
#' @param threshold_rate numeric : A value between 0 and 1 for the proportion of
#' the median consumption/income for SPL calculation
#' @param ppp numeric: PPP values inputted by the user (set to 1 if welfare values are already in dollars)
#' @param cpi numeric: CPI values inputted by the user (set to 1 if welfare values year is the reference year)
#'
#' @return numeric
#' @keywords internal
#'
#' @examples
#' wbpip::md_compute_spl(welfare = 1:2000, weight = rep(1, 2000))

md_compute_spl <- function(welfare,
                           weight,
                           threshold_rate = 0.5,
                           ppp = 1,
                           cpi = 1){

  o       <- order(welfare)
  welfare <- welfare[o]
  weight  <- weight[o]

  dt <- data.table::data.table(welfare = welfare,
                               weight = weight)

  #find the xth percentile of the weight variable
  dt[,cum_weight := cumsum(weight)]

  cum_median <- dt[,stats::quantile(cum_weight,
                                  probs = 0.5,
                                  na.rm = TRUE)][[1]]

  if(cum_median %in% dt$cum_weight){

    spl <- dt[cum_weight == cum_median, welfare]
    spl <- spl / ppp / cpi / 365
    spl <- 1 + threshold_rate * spl

    return(spl)
  } else if ((cum_median %in% dt$cum_weight) == FALSE) {

    dt[,indicator := findInterval(cum_weight, cum_median)]
    dt[,index := 1:.N]

    #find the numbers that bound median from both above and below
    upper_welf <- dt[indicator == 0, max(welfare)]
    lower_welf <- dt[indicator == 1, min(welfare)]
    spl <- (upper_welf + lower_welf)/2
    spl <- spl / ppp / cpi / 365
    spl <- 1 + threshold_rate * spl

    return(spl)

  }


}
