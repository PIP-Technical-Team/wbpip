#' Infer poverty line
#'
#' Infer poverty line for microdata.
#'
#' This function calculate percentiles corresponding to the specified share of
#' population (in percentages).
#'
#' @inheritParams compute_pip_stats
#' @param include logical: **TO BE DOCUMENTED**.
#'
#' @examples
#' wbpip:::md_infer_poverty_line(1:2000, weight = rep(1, 2000))
#' wbpip:::md_infer_poverty_line(1:2000,
#'   weight = rep(1, 2000),
#'   popshare = .2
#' )
#' wbpip:::md_infer_poverty_line(1:2000,
#'   weight = rep(1, 2000),
#'   popshare = .6
#' )
#' @return numeric
#' @keywords internal
md_infer_poverty_line <- function(welfare,
                                  weight,
                                  popshare = .5,
                                  include = FALSE) {

  if (anyNA(c(welfare, weight))) {
    cli::cli_abort("neither {.arg welfare} nor {.arg weight} can have {.var NAs}")
  }

  prob <- fcumsum(weight) / fsum(weight)
  ps <- lapply(popshare, \(.) {
    abs(prob - .) |>
      which.min()
  })

  # Weighted mean with the next available value in order to
  # guarantee inclusion in poverty calculation

  if (include) {

    pctile <-
      vapply(ps, \(.) {
        fmean(
          x = c(welfare[.], welfare[. + 1]),
          w = c(weight[.], weight[. + 1])
        )
      },
      numeric(1))


  } else {
    pctile <-
      vapply(ps, \(.) {
        fmean(welfare[.])
      },
      numeric(1))
  }


  return(pctile)
}
