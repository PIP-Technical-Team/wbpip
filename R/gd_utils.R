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

#' Return data according to format
#'
#' @param ld list of data
#' @param format character: either "dt" for data.table, "list" or "atomic" for a
#'   single numeric vector, whose names are corresponding selected Lorenz for
#'   each value.  Default is "dt"
#' @param var character: name of variable to be returned.
#'
#' @return data.table, list, or atomic vector
#' @keywords internal
return_format <-
  function(ld,
           var,
           povline = NULL,
           complete = FALSE,
           format = c("dt", "list", "atomic")) {

  format <- match.arg(format)

  inv_reduce <- function(x,f) {
    Reduce(f,x)
  }

#   ____________________________________________________
#   Early returns                                   ####
  if (FALSE) {
    return()
  }

#   ____________________________________________________
#   Computations                                     ####
  if (format == "list") {
    return(ld)
  }

  if (complete == TRUE) {
    cli_abort("{.field complete} is only available with {.field format} = 'list'")
  }

  dt <- ld |>
    inv_reduce(c) |>
    inv_reduce(c)

  pg <- dt[names(dt) == var] |>
    unlist()
  sl <- dt[names(dt) == "lorenz"] |>
    unlist()

  if (format == "dt") {
    dt <- data.table(povline   = povline,
                     V1        = pg,
                     lorenz    = sl)
    setnames(dt, "V1", var)
    return(dt)
  }

  if (format == "atomic") {
    names(pg) <- sl
    return(pg)
  }


#   ____________________________________________________
#   Return                                           ####
  return(TRUE)

}




#' Check parameters of get_gd functions
#'
#' @param lp list of parameters
#'
#' @return invisible TRUE
check_get_gd_fun_params <- function(lp) {

#   ____________________________________________________________________________
#   on.exit                                                                 ####
  on.exit({

  })

#   ____________________________________________________________________________
#   Defenses                                                                ####
  stopifnot( exprs = {

    }
  )

#   ____________________________________________________________________________
#   Early returns                                                           ####
  if (FALSE) {
    return()
  }

#   ____________________________________________________________________________
#   Computations                                                            ####

  nlp <- names(lp)

  ## welfare -----------


  ## welfare and params -----------
  if ( all(c("params", "welfare") %in% nlp)) {
    if (!is.null(lp$params) &&
        (!is.null(lp$welfare)  || !is.null(lp$population))) {
      cli_abort("You must specify either {.field params} or
                {.field welfare} and {.field population}")
    }
  }


  ## povline and popshare ----------
  if ( all(c("povline", "popshare") %in% nlp)) {
    if (!is.na(lp$povline) && !is.null(lp$popshare)) {
      cli_abort("You must specify either {.field povline} or
                {.field popshare}")
    }
  }


  # "Either `params` or `welfare` and `population` should be spefied" =
  #   (is.null(params) && !is.null(welfare) && !is.null(population)) ||
  #   (!is.null(params) && is.null(welfare) && is.null(population))
  #
  # "`params` should be a list from `get_gd_lorenz_params()`" =
  #   is.list(params) || is.null(params)
  #
  # "`complete` must be logical" =
  #   is.logical(complete)

  ## lorenz -----------
  if ( all(c("lorenz") %in% nlp)) {

    if (!is.null(lp$lorenz) && !lp$lorenz %in% c("lq", "lb")) {

      cli_abort("{.field lorenz} must be either 'lq' or 'lb', or
                {.code NULL} to let the algorithm select")
    }
  }


#   ____________________________________________________________________________
#   Return                                                                  ####
  return(invisible(TRUE))

}

