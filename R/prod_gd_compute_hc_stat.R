#' Computes headcount statistic (grouped)
#' Version used in prod
#'
#' Compute headcount for grouped data by selecting the best functional
#' fit for the Lorenz curve (either beta or quadratic).
#'
#' @param welfare numeric: Cumulative proportion of welfare held by that
#' proportion of the population (Lorenz Curve).
#' @param povline numeric: Poverty line.
#' @param population numeric: Cumulative proportion of population.
#' @param requested_mean numeric: Welfare mean.
#' @param popshare numeric: Share of population living below the poverty line.
#' Optional.
#' @param default_ppp numeric: Default purchasing power parity.
#' @param ppp numeric: PPP request by user.
#' @param p0 numeric: **TO BE DOCUMENTED**.
#'
#' @return list
#' @keywords internal
prod_gd_compute_hc_stat <- function(welfare,
                                    povline,
                                    population,
                                    requested_mean,
                                    popshare = NULL,
                                    default_ppp,
                                    ppp = NULL,
                                    p0 = 0.5) {

  # Adjust mean and median if different PPP value is provided
  if (!is.null(ppp)) {
    requested_mean <- requested_mean * default_ppp / ppp
  } else {
    ppp <- default_ppp
  }

  # Apply Lorenz quadratic fit ----------------------------------------------
  results_lq <- prod_gd_compute_hc_stat_lq(
    welfare = welfare,
    population = population,
    requested_mean = requested_mean,
    povline = povline,
    popshare = popshare,
    default_ppp = default_ppp,
    ppp = ppp,
    p0 = p0
  )

  # Apply Lorenz beta fit ----------------------------------------------
  results_lb <- prod_gd_compute_pip_stats_lb(
    welfare = welfare,
    population = population,
    requested_mean = requested_mean,
    povline = povline,
    popshare = popshare,
    default_ppp = default_ppp,
    ppp = ppp,
    p0 = p0
  )


  # Apply selection rules ---------------------------------------------------
  out <- prod_gd_select_lorenz_headcount(
    lq = results_lq,
    lb = results_lb
  )

  # Return only subset of variables
  # out <- out[c("headcount")]

  return(out)
}


#' Computes headcount (Lorenz quadratic)
#' Version used in production
#'
#' @inheritParams gd_compute_pip_stats
#' @return list
#' @keywords internal
prod_gd_compute_hc_stat_lq <- function(welfare,
                                       povline,
                                       population,
                                       requested_mean,
                                       popshare = NULL,
                                       default_ppp = NULL,
                                       ppp = NULL,
                                       p0 = 0.5) {

  # STEP 1: Prep data to fit functional form
  prepped_data <- create_functional_form_lq(
    welfare = welfare,
    population = population
  )

  # STEP 2: Estimate regression coefficients using LQ parameterization
  reg_results <- regres(prepped_data, is_lq = TRUE)
  reg_coef <- reg_results$coef

  A <- reg_coef[1]
  B <- reg_coef[2]
  C <- reg_coef[3]

  # OPTIONAL: Only when popshare is supplied
  # return poverty line if share of population living in poverty is supplied
  # intead of a poverty line
  if (!is.null(popshare)) {
    povline <- derive_lq(popshare, A, B, C) * requested_mean
  }

  # Boundary conditions (Why 4?)
  z_min <- requested_mean * derive_lq(0.001, A, B, C) + 4
  z_max <- requested_mean * derive_lq(0.980, A, B, C) - 4
  z_min <- if (z_min < 0) 0 else z_min

  results1 <- list(requested_mean, povline, z_min, z_max, ppp)
  names(results1) <- list("mean", "poverty_line", "z_min", "z_max", "ppp")

  # STEP 3: Estimate headcount based on identified parameters
  results2 <- prod_gd_estimate_lq_headcount(requested_mean, povline, p0, A, B, C)

  # STEP 4: Compute measure of regression fit
  results_fit <- gd_compute_fit_lq(welfare, population, results2$headcount, A, B, C)

  res <- c(results1, results2, results_fit, reg_results)

  return(res)
}

#' Computes headcount (Lorenz beta)
#' Version used in production
#'
#' @inheritParams gd_compute_pip_stats
#' @return list
#' @keywords internal
prod_gd_compute_hc_stat_lb <- function(welfare,
                                       povline,
                                       population,
                                       requested_mean,
                                       popshare = NULL,
                                       default_ppp,
                                       ppp = NULL,
                                       p0 = 0.5) {

  # STEP 1: Prep data to fit functional form
  prepped_data <- create_functional_form_lb(
    welfare = welfare,
    population = population
  )

  # STEP 2: Estimate regression coefficients using LB parameterization
  reg_results <- regres(prepped_data, is_lq = FALSE)
  reg_coef <- reg_results$coef

  A <- reg_coef[1]
  B <- reg_coef[2]
  C <- reg_coef[3]

  # OPTIONAL: Only when popshare is supplied
  # return poverty line if share of population living in poverty is supplied
  # intead of a poverty line

  if (!is.null(popshare)) {
    povline <- derive_lb(popshare, A, B, C) * requested_mean
  }

  # Boundary conditions (Why 4?)
  z_min <- requested_mean * derive_lb(0.001, A, B, C) + 4
  z_max <- requested_mean * derive_lb(0.980, A, B, C) - 4
  z_min <- if (z_min < 0) 0 else z_min

  results1 <- list(requested_mean, povline, z_min, z_max, ppp)
  names(results1) <- list("mean", "poverty_line", "z_min", "z_max", "ppp")

  # STEP 3: Estimate poverty measures based on identified parameters
  results2 <- prod_gd_estimate_lb(requested_mean, povline, p0, A, B, C)

  # STEP 4: Compute measure of regression fit
  results_fit <- gd_compute_fit_lb(welfare, population, results2$headcount, A, B, C)

  res <- c(results1, results2, results_fit, reg_results)

  return(res)
}



#' Estimates headcount from Beta Lorenz fit
#' Version used in production
#'
#' @param mean numeric: Welfare mean.
#' @param povline numeric: Poverty line.
#' @param p0 numeric: **TO BE DOCUMENTED**.
#' @param A numeric: Lorenz curve coefficient. Output of
#'   `regres()$coef[1]`.
#' @param B numeric: Lorenz curve coefficient. Output of
#'   `regres()$coef[2]`.
#' @param C numeric: Lorenz curve coefficient. Output of
#'   `regres()$coef[3]`.
#'
#' @return list
#' @keywords internal
prod_gd_estimate_lb <- function(mean, povline, p0, A, B, C) {

  # Compute poverty stats
  headcount <- gd_compute_headcount_lb(mean, povline, A, B, C)

  # Check validity
  validity <- check_curve_validity_lb(headcount, A, B, C)

  out <- list(
    headcount = pov_stats$headcount,
    is_normal = validity$is_normal,
    is_valid = validity$is_valid
  )

  return(out)
}

#' Select best Lorenz fit (headcount)
#' Version used in production
#'
#' Select best Lorenz fit and adjust the returned statistics if needed.
#'
#' @param lq list: Results from Lorenz Quadratic functional form. output of
#'   `gd_compute_pip_stats_lq()`.
#' @param lb list: Results from Lorenz Beta functional form. output of
#'   `gd_compute_pip_stats_lb()`.
#'
#' @return numeric
#' @keywords internal
prod_gd_select_lorenz_headcount <- function(lq, lb) {

  # Set default value
  is_valid <- lq[["is_valid"]] | lb[["is_valid"]]
  is_normal <- lq[["is_normal"]] | lb[["is_normal"]]

  # Selection of Lorenz fit for poverty statistics
  use_lq_for_pov <- use_lq_for_poverty(
    lq = lq,
    lb = lb
  )

  # Retrieve poverty statistics
  headcount <- gd_retrieve_headcount(
    lq = lq,
    lb = lb,
    is_normal = is_normal,
    use_lq_for_pov = use_lq_for_pov
  )

  return(list(headcount = headcount))
}

#' Estimate headcount from Quadratic Lorenz fit
#' Version used in production
#'
#' @param mean numeric: Welfare mean.
#' @param povline numeric: Poverty line.
#' @param p0 numeric: **TO BE DOCUMENTED**.
#' @param A numeric: Lorenz curve coefficient. Output of
#'   `regres_lq()$coef[1]`.
#' @param B numeric: Lorenz curve coefficient. Output of
#'   `regres_lq()$coef[2]`.
#' @param C numeric: Lorenz curve coefficient. Output of
#'   `regres_lq()$coef[3]`.
#'
#' @return list
#' @keywords internal
prod_gd_estimate_lq_headcount <- function(mean, povline, p0, A, B, C) {

  # Compute key numbers from Lorenz quadratic form
  # Theorem 3 from original Lorenz quadratic paper
  e <- -(A + B + C + 1) # e = -(A + B + C + 1): condition for the curve to go through (1, 1)
  m <- (B^2) - (4 * A) # m < 0: condition for the curve to be an ellipse (m is called alpha in paper)
  n <- (2 * B * e) - (4 * C) # n is called Beta in paper
  r <- (n^2) - (4 * m * e^2) # r is called K in paper

  # Check validity
  validity <- check_curve_validity_lq(A, B, C, e, m, n, r)
  if (validity$is_valid == FALSE & validity$is_normal == FALSE) {
    return(NA_real_)
  }

  # Compute headcount
  r <- sqrt(r)
  headcount <- gd_compute_headcount_lq(mean, povline, B, m, n, r)

  return(list(
    headcount = headcount,
    is_normal = validity$is_normal,
    is_valid = validity$is_valid
  ))
}

#' Algorithm to retrieve correct headcount
#'
#' @inheritParams gd_select_lorenz
#' @param is_normal logical: Whether at least one of the Lorenz fit is normal
#' @param is_lq_for_pov logical: Whether to use LQ (TRUE) or Beta fit (FALSE)
#'
#' @return numeric
#' @keywords internal
gd_retrieve_headcount <- function(lq,
                                  lb,
                                  is_normal,
                                  use_lq_for_pov) {
  if (!is_normal) {
    return(NA_real_)
  }
  if (use_lq_for_pov) {
    headcount <- lq[["headcount"]]
  } else {
    headcount <- lb[["headcount"]]
  }
  # fix abnormal values
  if (headcount < 0) {
    return(NA_real_)
  }
  if (headcount > 1) {
    return(0.99999)
  }

  return(headcount)
}
