#' Compute poverty line in Local Currency Unit (LCU)
#'
#' Compute the LCU poverty line for microdata.
#'
#' @inheritParams compute_pip_stats
#' @param data_mean numeric: Data mean.
#'
#' @return list
#' @keywords internal
#' @examples
#'
#' # Load and clean example data
#' data("md_ABC_2000_income")
#' df <- wbpip:::md_clean_data(
#'   md_ABC_2000_income,
#'   welfare = 'welfare',
#'   weight = 'weight')$data
#'
#' # Compute LCU poverty line
#' res <-  wbpip:::md_compute_povline_lcu(
#'   df$welfare, df$weight,
#'   popshare = NULL,
#'   mean = 5000,
#'   data_mean = 4000)
#' str(res)
md_compute_povline_lcu <- function(welfare,
                                   povline,
                                   weight,
                                   popshare,
                                   mean,
                                   data_mean) {
  if (!is.null(popshare)) {
    # Infer poverty line from share of population living in poverty
    pl_lcu <- md_infer_poverty_line(
      welfare = welfare,
      weight = weight,
      popshare = popshare
    )

    povline <- pl_lcu * mean / data_mean
  } else {
    # Convert user defined international poverty line in Local Currency Units
    pl_lcu <- povline * data_mean / mean
  }

  return(list(
    povline_lcu = pl_lcu,
    povline     = povline
  ))
}
