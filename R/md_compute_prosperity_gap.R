#' Compute Prosperity Gap
#'
#' Compute prosperity gap using microdata.
#'
#' @inheritParams compute_pip_stats
#' @return numeric
#' @export
#'
#' @examples
md_compute_prosperity_gap <- function(
    welfare, weight, povline_lcu, cons_floor = c(0.5)
){

  # Bottom censor using consumption floor
  if(!is.null(cons_floor)){

    welfare[welfare < cons_floor] <- cons_floor

  }

  # Find for each individual
  pg_ind <- povline_lcu/welfare

  # weighted average
  pg <- weighted.mean(x = pg_ind, w = weight)

  # return
  return(pg)


}


