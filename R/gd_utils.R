#' Perform linear regression on Lorenz formatted input
#'
#' `regres()` performs linear regression on lorenz formatted input (Beta or
#' Quadratic). There is no intercept in the regression. The coefficients of
#' regressions are estimated by ordinary least squares.
#'
#' @param data list: Output of `create_functional_form_lq()` or
#' `create_functional_form_lb()`.
#' @param is_lq logical: TRUE if Lorenz Quadratic, FALSE if Beta Lorenz.
#'
#' @return list
#' @export
#' @examples
#' # Beta Lorenz
#' lb_data <- wbpip:::create_functional_form_lb(grouped_data_ex2$welfare,
#' grouped_data_ex2$weight)
#' lb_res <- wbpip:::regres(lb_data)
#'
#' # Quadratic Lorenz
#' lq_data <- wbpip:::create_functional_form_lq(grouped_data_ex2$welfare,
#' grouped_data_ex2$weight)
#' lq_res <- wbpip:::regres(lq_data)
#'
regres <- function(data, is_lq = TRUE) {

  y <- data$y
  X <- data$X

  n <- length(y)
  k <- ncol(X)

  # Run regression
  res <- stats::.lm.fit(y = y, x = X)

  # Calculate stats
  ymean <- sum(y) / n
  sst <- sum((y - ymean)^2) # sum of square total
  coef <- res$coefficients # regression coefs
  residuals <- res$residuals # residulas
  sse <- sum(residuals^2) # sum of square error
  r2 <- 1 - sse / sst # R-square (This is the R2 formula for models with an intercept)
  mse <- sse / (n - k) # Mean squared error
  s2 <- as.vector((residuals %*% residuals) / (n - k))
  se <- sqrt(s2 * (diag(MASS::ginv(t(X) %*% X)))) # Standard error

  # REVIEW:
  # Why exp() if isLQ == FALSE?
  if (!is_lq) {
    coef[1] <- exp(coef[1])
  }

  return(list(
    ymean = ymean,
    sst = sst,
    coef = coef,
    sse = sse,
    r2 = r2,
    mse = mse,
    se = se
  ))
}
