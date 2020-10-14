

#' Prepares data for Lorenz Quadratic regression
#'
#' @description  Prepares data for regression on L(1-L) on (P^2-L), L(P-1) and
#' (P-L). The last observation of (p,l), which by construction has the value
#' (1, 1), is excluded since the functional form for the Lorenz curve already
#' forces it to pass through the point (1, 1). Equation 15 in Lorenz Quadratic
#' original paper.
#'
#' @param lorenz_pop numeric: Population vector from empirical Lorenz curve
#' @param lorenz_welfare numeric: Welfare vector from empirical Lorenz curve
#'
#' @return data.frame
#'
#' @seealso \href{https://EconPapers.repec.org/RePEc:eee:econom:v:40:y:1989:i:2:p:327-338}{Original quadratic Lorenz curve paper}
#' @seealso \href{https://www.sciencedirect.com/science/article/abs/pii/S0304407613000158?via%3Dihub}{Corrigendum to Elliptical Lorenz Curves}

create_functional_form_lq <- function(lorenz_pop,
                                      lorenz_welfare) {
  # CHECK inputs
  assertthat::assert_that(is.numeric(lorenz_pop))
  assertthat::assert_that(is.numeric(lorenz_welfare))
  assertthat::assert_that(length(lorenz_pop) == length(lorenz_welfare))
  assertthat::assert_that(length(lorenz_pop) > 1)

  # Remove last observation (the functional form for the Lorenz curve already forces
  # it to pass through the point (1, 1)
  nobs <- length(lorenz_pop) - 1
  lorenz_pop <- lorenz_pop[1:nobs]
  lorenz_welfare <- lorenz_welfare[1:nobs]

  # L(1-L)
  y <- lorenz_welfare * (1 - lorenz_welfare)
  # (P^2-L)
  x1 <- lorenz_pop^2 - lorenz_welfare
  # L(P-1)
  x2 <- lorenz_welfare * (lorenz_pop - 1)
  # P-L
  x3 <- lorenz_pop - lorenz_welfare

  out <- data.frame(y, x1, x2, x3, stringsAsFactors = FALSE)

  return(out)
}


#' Returns the first derivative of the quadratic Lorenz
#'
#' `derive_lq()` returns the first derivative of the quadratic Lorenz curves
#' with c = 1. General quadratic form: ax^2 + bxy + cy^2 + dx + ey + f = 0
#'
#' @param x numeric: point on curve
#' @param A numeric vector: lorenz curve coefficient
#' @param B numeric vector: lorenz curve coefficient
#' @param C numeric vector: lorenz curve coefficient
#'
#' @return numeric
#'
derive_lq <- function(x, A, B, C) {
  e <- -(A + B + C + 1)
  m <- (B^2) - (4 * A)
  n <- (2 * B * e) - (4 * C)
  tmp <- (m * x^2) + (n * x) + (e^2)
  tmp <- ifelse(tmp < 0, 0, tmp)

  # Formula for first derivative of GQ Lorenz Curve
  val <- -(0.5 * B) - (0.25 * (2 * m * x + n) / sqrt(tmp))

  return(val)
}

#' Check validity of Lorenz Quadratic fit
#'
#' `check_curve_validity_lq()` checks the validity of the Lorenz Quadratic fit
#'
#' @param A numeric: First regression coefficient
#' @param B numeric: Second regression coefficient
#' @param C numeric: Third regression coefficient
#' @param e numeric: e = -(A + B + C + 1): condition for the curve to go through
#' (1, 1)
#' @param m numeric: m = (B^2) - (4 * A). m < 0: condition for the curve to be
#' an ellipse (m is called alpha in paper)
#' @param n numeric: n = (2 * B * e) - (4 * C). n is called Beta in paper
#' @param r r = (n^2) - (4 * m * e^2). r is called K in paper
#'
#' @return list
#'
#' @seealso \href{https://EconPapers.repec.org/RePEc:eee:econom:v:40:y:1989:i:2:p:327-338}{Original quadratic Lorenz curve paper}
#' @seealso \href{https://www.sciencedirect.com/science/article/abs/pii/S0304407613000158?via%3Dihub}{Corrigendum to Elliptical Lorenz Curves}
#' @seealso \href{https://www.ifpri.org/cdmref/p15738coll2/id/125673}{
#' Computational Tools For Poverty Measurement And Analysis}
#'
check_curve_validity_lq <- function(A, B, C, e, m, n, r) {

  is_normal <- FALSE
  is_valid <- FALSE

  # r needs to be > 0 because need to extract sq root
  if (r < 0) {return(list(is_normal = is_normal,
                          is_valid = is_valid))}

  if (e > 0 || C < 0) {
    return(list(is_normal = is_normal,
                is_valid = is_valid))
  }

  # Failure conditions for checking theoretically valid Lorenz curve
  # Found in section 4 of Datt computational tools paper
  cn1 <- n^2
  cn3 <- cn1 / (4 * e^2)

  if (!((m < 0) |
        ((m > 0) & (m < cn3) & (n >= 0)) |
        ((m > 0) & (m < -n/2) & (m < cn3)))) {
    return(list(is_normal = is_normal,
                is_valid = is_valid))
  }

  is_normal <- TRUE
  is_valid <- (A + C) >= 0.9

  return(list(is_normal = is_normal,
              is_valid = is_valid))

}

#' Compute gini index from Lorenz Quadratic fit
#'
#' `gd_compute_gini_lq()` computes the gini index from a Lorenz Quadratic fit
#'
#' @param A numeric: First regression coefficient
#' @param B numeric: Second regression coefficient
#' @param C numeric: Third regression coefficient
#' @param e numeric: e = -(A + B + C + 1): condition for the curve to go through
#' (1, 1)
#' @param m numeric: m = (B^2) - (4 * A). m < 0: condition for the curve to be
#' an ellipse (m is called alpha in paper)
#' @param n numeric: n = (2 * B * e) - (4 * C). n is called Beta in paper
#' @param r r = (n^2) - (4 * m * e^2). r is called K in paper
#'
#' @return numeric
#'
#' @seealso \href{https://www.ifpri.org/cdmref/p15738coll2/id/125673}{
#' Computational Tools For Poverty Measurement And Analysis}
gd_compute_gini_lq <- function(A, B, C, e, m, n, r) {

  # For the GQ Lorenz curve, the Gini formula are valid under the condition A+C>=1
  # P.isValid <- (A + C) >= 0.9
  # P.isNormal <- TRUE

  e1 <- abs(A + C - 1)
  e2 <- 1 + (B / 2) + e

  tmp1 <- n * (B + 2) / (4 * m)
  tmp2 <- (r^2) / (8 * m)
  tmp3 <- (2 * m) + n

  if (m > 0) {
    # tmpnum <- tmp3 + 2 * sqrt(m) * abs(e)
    # tmpden <- n - 2 * abs(e) * sqrt(m)

    # Formula from Datt paper
    # CHECK that code matches formulas in paper
    gini <- e2 + (tmp3/(4 * m)) * e1 - (n*abs(e) / (4 * m)) - ((r^2) / (8 * sqrt(m))) * log(abs(((tmp3 + (2 * sqrt(m) * e1)))/(n + (2 * sqrt(m) * abs(e)))))
    #P.gi <- (e/2) - tmp1 - (tmp2 * log(abs(tmpnum/tmpden)) / sqrt(m))

  } else {
    tmp4 <- ((2*m) + n) / r
    tmp4 <- ifelse(tmp4 < -1, -1, tmp4)
    tmp4 <- ifelse(tmp4 > 1, 1, tmp4)

    # Formula does not match with paper
    gini <- e2 + (tmp3/(4*m)) * e1 - (n*abs(e)/(4*m)) + (tmp2 * (asin(tmp4) - asin(n/r)) / sqrt(-m))
    # P.gi <- (e/2) - tmp1 + ((tmp2 * (asin(tmp4) - asin(n/r))) / sqrt(-m))
  }

  return(gini)

}

#' Solves for quadratic Lorenz curves
#'
#' `value_at_lq()`solves for quadratic Lorenz curves with c = 1
#' General quadratic form: ax^2 + bxy + cy^2 + dx + ey + f = 0
#'
#' @param x numeric: point on curve
#' @param A numeric vector: First lorenz curve coefficient
#' @param B numeric vector: Second lorenz curve coefficient
#' @param C numeric vector: Third lorenz curve coefficient
#'
#' @return numeric
#'
value_at_lq <- function(x, A, B, C) {
  e <- -(A + B + C + 1)
  m <- (B^2) - (4 * A)
  n <- (2 * B * e) - (4 * C)
  temp <- (m * x^2) + (n * x) + (e^2)
  temp <- ifelse(temp < 0, 0, temp)

  # Solving the equation of the Lorenz curve
  estle <- -0.5 * ((B * x) + e + sqrt(temp))

  return(estle)
}

#' Computes MLD from Lorenz Quadratic fit
#'
#' `gd_compute_mld_lq()` computes the Mean Log deviation (MLD) from a Lorenz
#' Quadratic fit
#'
#' @param dd numeric
#' @param A numeric vector: lorenz curve coefficient
#' @param B numeric vector: lorenz curve coefficient
#' @param C numeric vector: lorenz curve coefficient
#'
#' @return numeric
#'
gd_compute_mld_lq <- function(dd, A, B, C) {
  x1 <- derive_lq(0.0005, A, B, C)
  gap <- 0
  mld <- 0
  if (x1 == 0) {
    gap <- 0.0005
  }
  else {
    mld <- log(x1) * 0.001
  }
  x1 <- derive_lq(0, A, B, C)
  for(xstep in seq(0, 0.998, 0.001)) {
    x2 <- derive_lq(xstep + 0.001, A, B, C)
    if ((x1 <= 0) || (x2 <= 0)) {
      gap <- gap + 0.001
      if (gap > 0.5) {
        return(-1)
      }
    }
    else {
      gap <- 0
      mld <- mld + (log(x1) + log(x2)) * 0.0005
    }
    x1 <- x2
  }
  return(-mld)
}

#' Compute quantiles from Lorenz Quandratic fit
#'
#' `gd_compute_quantile_lq()` computes quantiles from a Lorenz Quadratic fit.
#'
#' @param A numeric vector: lorenz curve coefficient
#' @param B numeric vector: lorenz curve coefficient
#' @param C numeric vector: lorenz curve coefficient
#'
#' @return numeric
#'
gd_compute_quantile_lq <- function(A, B, C, n_quantile = 10) {
  vec <- vector(mode = "numeric", length = n_quantile)
  x1 <- 1 / n_quantile
  q <- 0
  lastq <- 0
  for (i in seq_len(n_quantile - 1)) {
    q <- value_at_lq(x1, A, B, C)
    v <- q - lastq
    vec[i] <- v
    lastq <- q
    x1 <- x1 + 1 / n_quantile
  }
  vec[n_quantile] <- 1 - lastq

  return(vec)
}

#'  Computes Watts Index from Quadratic Lorenz fit
#'
#' `gd_compute_watts_lq()` computes Watts Index from Quadratic Lorenz fit
#' The first distribution-sensitive poverty measure was proposed in 1968 by Watts
#' It is defined as the mean across the population of the proportionate poverty
#' gaps, as measured by the log of the ratio of the poverty line to income,
#' where the mean is formed over the whole population, counting the nonpoor as
#' having a zero poverty gap.
#'
#' @param headcount numeric: headcount index
#' @param mu numeric
#' @param povline numeric: poverty line
#' @param dd numeric
#' @param A numeric vector: lorenz curve coefficient
#' @param B numeric vector: lorenz curve coefficient
#' @param C numeric vector: lorenz curve coefficient
#'
#' @return numeric
#' @export
#'
#' @examples
#' watt_index_lq(headcount, dd, A, B, C)
#'
gd_compute_watts_lq <- function(headcount, mu, povline, dd, A, B, C) {
  if (headcount <= 0) {
    return(0)
  }

  # x1 = x2 = xstep = xend = gap <- 0
  x1 <- 0
  x2 <- 0
  xstep <- 0
  xend <- 0
  gap <- 0
  snw <- headcount * dd
  watts <- 0

  x1 <- derive_lq(snw / 2, A, B, C)
  if (x1 <= 0) {
    gap <- snw / 2
  } else {
    watts <- log(x1) * snw
  }
  xend <- headcount - snw
  x1 <- derive_lq(0, A, B, C)
  # Number of steps seems to be different from what happens in .Net codebase
  for (xstep in seq(0, xend, by = snw)) {
    x2 <- derive_lq(xstep + snw, A, B, C)
    if ((x1 <= 0) || (x2 <= 0)) {
      gap <- gap + snw
      if (gap > 0.05) {
        return(NA)
      }
    } else {
      gap <- 0
      watts <- watts + (log(x1) + log(x2)) * snw * 0.5
    }
    x1 <- x2
  }
  if ((mu != 0) && (watts != 0)) {
    x1 <- povline / mu
    if (x1 > 0) {
      watts <- log(x1) * headcount - watts
      if (watts > 0) {
        return(watts)
      }
    }
    return(NA)
  }
}

#' Computes polarization index from Quadratic Lorenz fit
#'
#' @param mean numeric: Welfare mean
#' @param p0 numeric: To document
#' @param dcm numeric: To document
#' @param A numeric: Lorenz curve coefficient
#' @param B numeric: Lorenz curve coefficient
#' @param C numeric: Lorenz curve coefficient
#'
#' @return numeric
#'
gd_compute_polarization_lq <- function(mean,
                                       p0,
                                       dcm,
                                       A, B, C) {

  pol <- 2 - (1 / p0) +
    (dcm - (2 * value_at_lq(p0, A, B, C) * mean)) /
    (p0 * mean * derive_lq(p0, A, B, C))

  return(pol)
}
