#' Mean Log Deviation
#'
#' Given a vector of weights and welfare, this functions computes the
#' Mean Log Deviation (MLD).
#'
#' @inheritParams md_compute_dist_stats
#' @param alpha the parameter regulating the importance of the distance between incomes at different parts of the income
#' distribution
#' @param drop logical, if TRUE, drop all values less than or equal to 0
#' @return numeric
#' @examples
#' wbpip:::md_compute_mld(welfare = 1:2000, weight = rep(1, 2000))
#' @keywords internal
md_compute_mld <- function(welfare, weight, mean = NULL) {

  # Compute MLD
  if (is.null(mean)) {
    mean <- collapse::fmean(x = welfare, w = weight)
  }

  welfare[welfare <= 0] <- 1 # this should be done before the mean
  deviation <- log(mean / welfare)
  mld <- collapse::fmean(
    x = deviation,
    w = weight
  )
  return(mld)
}

md_compute_mld2 <- function(welfare,
                            weight,
                            alpha = 0,
                            mean = NULL,
                            drop = TRUE){


  # handling the zeros
  if (drop == FALSE) {

    # Compute MLD
    if (is.null(mean)) {
      mean <- collapse::fmean(x = welfare, w = weight)
    }

    welfare[welfare <= 0] <- 1

  } else {

    to_keep <- which(welfare > 0)
    welfare <- welfare[to_keep]
    weight  <- weight[to_keep]

    # Compute MLD
    if (is.null(mean)) {
      mean <- collapse::fmean(x = welfare, w = weight)
    }

  }


  N <- length(welfare)

  if (alpha == 0){

    deviation <- log(mean / welfare)
    mld <- collapse::fmean(
      x = deviation,
      w = weight
    )

  } else if (alpha == 1){
    value <- (welfare / mean) * log(welfare / mean)

    mld <- collapse::fmean(
      x = value,
      w = weight
    )
  } else {

      value <- ((welfare / mean)^alpha) - 1
      mld <- sum(value, na.rm = TRUE) * (1/(N*alpha*(alpha - 1)))

  }

  return(mld)
}




