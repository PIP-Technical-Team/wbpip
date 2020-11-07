#' Prepares data for Lorenz Quadratic regression
#'
#' @description  Prepares data for regression of L(1-L) on (P^2-L), L(P-1) and
#' (P-L). The last observation of (p,l), which by construction has the value
#' (1, 1), is excluded since the functional form for the Lorenz curve already
#' forces it to pass through the point (1, 1). Equation 15 in Lorenz Quadratic
#' original paper.
#' Then it performs linear regression on lorenz formatted input (Beta or
#' Quadratic). There is no intercept in the regression. The coefficients of
#' regressions are estimated by ordinary least squares.
#'
#' @param population numeric: Population vector from empirical Lorenz curve
#' @param welfare numeric: Welfare vector from empirical Lorenz curve
#'
#' @return data.frame
#' @export
#'
#' @seealso \href{https://EconPapers.repec.org/RePEc:eee:econom:v:40:y:1989:i:2:p:327-338}{Original quadratic Lorenz curve paper}
#' @seealso \href{https://www.sciencedirect.com/science/article/abs/pii/S0304407613000158?via%3Dihub}{Corrigendum to Elliptical Lorenz Curves}

create_functional_form_lq <- function(welfare,
                                      population) {
  # CHECK inputs
  assertthat::assert_that(is.numeric(population))
  assertthat::assert_that(is.numeric(welfare))
  assertthat::assert_that(length(population) == length(welfare))
  assertthat::assert_that(length(population) > 1)



  #--------- functional form ---------

  # Remove last observation (the functional form for the Lorenz curve already forces
  # it to pass through the point (1, 1)
  nobs <- length(population) - 1
  population <- population[1:nobs]
  welfare <- welfare[1:nobs]

  # L(1-L)
  y <- welfare * (1 - welfare)
  # (P^2-L)
  x1 <- population^2 - welfare
  # L(P-1)
  x2 <- welfare * (population - 1)
  # P-L
  x3 <- population - welfare

  df <- data.frame(y, x1, x2, x3, stringsAsFactors = FALSE)


  #--------- regression ---------

  res <- stats::lm(y ~ x1 + x2 + x3 - 1, data = df)

  coef_stat <- broom::tidy(res)
  fitness   <- broom::glance(res)

  ymean <- mean(df[["y"]])
  sst   <- sum((df[["y"]] - ymean)^2)    # sum of square total
  coef  <- unname(res[["coefficients"]]) # regression coefs
  sse   <- sum(res$residuals^2)          # sum of square error
  r2    <- 1 - sse / sst                 # R-square (R2 formula for models with an intercept)
  mse   <- sse / (nrow(df) - length(coef))      # Mean squared error
  se    <- unname(sqrt(diag(stats::vcov(res)))) # Standard error

  # REVIEW:
  # Where is is_lq defined?
  # Why exp() if is_lq == FALSE?
  # if (!is_lq) {
  #
  #   coef[1] <- exp(coef[1])
  #
  # }

  return(list(ymean     = ymean,
              sst       = sst,
              coef      = coef,
              sse       = sse,
              r2        = r2,
              mse       = mse,
              se        = se,
              coef_stat = coef_stat,
              fitness   = fitness))

  return(out)
}
