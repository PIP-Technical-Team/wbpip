#' Weighted median
#'
#' @param x Numeric vector
#' @param weight Numeric weight vector
#'
#' @return Weighted median of x
#' @export
#'
#' @examples
get_weighted_median <- function(
    x, weight
){
  # Input checks
  stopifnot(length(x) == length(weight))

  # reorder both according to x
  weight <- weight[order(x)]
  x <- x[order(x)]

  prob <- cumsum(weight)/sum(weight)
  ps <- which(abs(prob - .5) == min(abs(prob - .5)))


  # return
  return(x[ps])


}
