#' Performs linear regression on lorenz formatted input
#'
#' `regres()` performs linear regression on lorenz formatted input (Beta or
#' Quadratic). There is no intercept in the regression. The coefficients of
#' regressions are estimated by ordinary least squares.
#'
#' @param df dataframe: Output of `create_functional_form_lq()` or
#' `create_functional_form_lb()`
#'
#' @return list
#'

regres <- function(df, is_lq = TRUE) {
  #CHECK inputs
  assertthat::are_equal(ncol(df), 4)
  assertthat::are_equal(names(df), c("y", "x1", "x2", "x3"))

  res <- stats::lm(y ~ x1 + x2 + x3 - 1, data = df)

  ymean <- mean(df[["y"]])
  sst   <- sum((df[["y"]] - ymean)^2) # sum of square total
  coef  <- unname(res[["coefficients"]]) # regression coefs
  sse   <- sum(res$residuals^2) # sum of square error
  r2    <- 1 - sse / sst # R-square (This is the R2 formula for models with an intercept)
  mse   <- sse / (nrow(df) - length(coef)) # Mean squared error
  se    <- unname(sqrt(diag(stats::vcov(res)))) # Standard error
  # REVIEW:
  # Where is isLQ defined?
  # Why exp() if isLQ == FALSE?
  if (!is_lq) {coef[1] <- exp(coef[1])}

  return(list(ymean = ymean,
              sst = sst,
              coef = coef,
              sse = sse,
              r2 = r2,
              mse = mse,
              se = se))
}
