#' compute median for microdata
#'
#' @param welfare welfare vector
#' @param weight population weights vector
#'
#' @return scalar
#' @keywords internal
md_compute_median <- function(welfare,weight) {
  median <- collapse::fmedian(x = welfare,
                              w = weight)
  return(median)
}
