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
    welfare, weight, povline_lcu
){

  # Find for each individual
  pg_ind <- povline_lcu/welfare

  # Censor
  pg_ind[pg_ind < 1] <- 1

  # weighted average
  pg <- weighted.mean(x = pg_ind, w = weight)

  # return
  return(pg)


}
