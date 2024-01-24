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
#' @keywords internal
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

#' Check for NA values in vector
#'
#' It is used for now on Lorenz curves equation
#'
#' @param x
#' @return A message if any values are NA or Inf
#' @keywords internal
check_NA_Inf_values <- function(x){

  if((anyNA(x)==TRUE | any(is.infinite(x))==TRUE)){
    cli::cli_abort("x should not contain NA or Inf values")
  }

  return(invisible(TRUE))
}

#' Check for negative values in vector
#'
#' It is used for now on Lorenz curves equation
#'
#' @param x
#' @return A message if any values are negative
#' @keywords internal
check_neg_values <- function(x){

  if(any(x<0)==TRUE){
    cli::cli_abort("All values in x should be positive")
  }

  return(invisible(TRUE))
}
