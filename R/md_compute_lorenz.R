#' Lorenz curve
#'
#' Compute the Lorenz curve for microdata.
#'
#' Given a vector of weights and welfare, this functions computes the Lorenz
#' curve.
#'
#' @param welfare numeric: A vector of income or consumption values.
#' @param weight numeric: A vector of weights. Default is a vector of ones,
#'   `rep(1, length(welfare))`.
#' @param nbins numeric: number of points on the Lorenz curve. if `NULL` the
#'   returning  Lorenz curve would be the  length of the original welfare vector
#'   minus the number of `NAs` of different observations in  `welfare` and
#'   `weight` plus 1 to account for the (0,0) intercept. Default is `100` for
#'   `length(welfare) > 1000` and `20` otherwise.
#' @param type numeric: integer. Quantile types 5-9. See [quantile]. The default
#'   method is type 7.
#'
#' @examples
#' wbpip:::md_compute_lorenz(welfare = 1:2000, weight = rep(1, 2000))
#' @return data.frame
#' @export
md_compute_lorenz <- function(welfare,
                              weight = rep(1, length(welfare)),
                              nbins = if (length(welfare) > 1000) 100 else 20,
                              type  = 7L) {


  # deal with NAs -----
  if (anyNA(welfare)) {
    ina      <- !is.na(welfare)
    weight   <- weight[ina]
    welfare  <- as.numeric(welfare)[ina]
  }

  if (anyNA(weight)) {
    ina      <- !is.na(weight)
    weight   <- weight[ina]
    welfare  <- as.numeric(welfare)[ina]
  }

  # Sort data ------

  if (is.unsorted(welfare)) {
    o       <- collapse::radixorder(welfare)
    welfare <- welfare[o]
    weight  <- weight[o]
  }

  # Compute Lorenz curve  -----
  weighted_welfare     <- weight * welfare

  p <- collapse::fcumsum(weight)/collapse::fsum(weight)
  L <- collapse::fcumsum(weighted_welfare)/collapse::fsum(weighted_welfare)

  # Intercept ----
  p       <- c(0,p)
  L       <- c(0,L)
  welfare <- c(0, welfare)

  # get quantiles  ----
  if (!is.null(nbins)) {
    probs   <- seq(1/nbins,1, 1/nbins)

    welfare <- collapse::.quantile(welfare,
                                   probs = probs,
                                   type = type)
    L       <- collapse::.quantile(L,
                                  probs = probs,
                                  type = type)
    p       <- collapse::.quantile(p,
                                   probs = probs,
                                   type = type)
  }

  # return ----------

  lorenz <- data.frame(
    welfare        = welfare,
    lorenz_welfare = L,
    lorenz_weight  = p
  )
  return(lorenz)

}
