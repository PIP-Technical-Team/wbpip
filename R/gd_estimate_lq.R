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
#' #' @seealso \href{https://EconPapers.repec.org/RePEc:eee:econom:v:40:y:1989:i:2:p:327-338}{Original quadratic Lorenz curve paper}
#' @seealso \href{https://www.sciencedirect.com/science/article/abs/pii/S0304407613000158?via%3Dihub}{Corrigendum to Elliptical Lorenz Curves}
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

  cn1 <- n^2
  cn2 <- 4 * m * e^2 # formula does not match with paper
  cn3 <- cn1 / (4 * e^2)

  # Failure conditions for checking theoretically valid Lorenz curve
  if (!((m < 0) ||
        ((m > 0) && (m < cn3) && (n >= 0)) ||
        ((m > 0) && (m < -n/2) && (m < cn3)))) {
    return(list(is_normal = is_normal,
                is_valid = is_valid))
  }

  is_normal <- TRUE
  is_valid <- (A + C) >= 0.9

  return(list(is_normal = is_normal,
              is_valid = is_valid))

}
