#' Computes poverty statistics (Lorenz beta)
#' Version used in production
#'
#' Compute poverty statistics for grouped data using the beta functional form of
#' the Lorenz qurve.
#'
#' @inheritParams gd_compute_pip_stats
#' @return list
#' @keywords internal
prod_gd_compute_pip_stats_lb <- function(welfare,
                                    povline,
                                    population,
                                    requested_mean,
                                    popshare = NULL,
                                    default_ppp = NULL,
                                    ppp = NULL,
                                    p0 = 0.5) {

  # Adjust mean if different PPP value is provided
  if (!is.null(ppp)) {
    requested_mean <- requested_mean * default_ppp / ppp
  } else {
    ppp <- default_ppp
  }
  # STEP 1: Prep data to fit functional form
  prepped_data <- create_functional_form_lb(welfare = welfare,
                                            population = population)

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
  z_min <- ifelse(z_min < 0, 0, z_min)

  results1 <- list(requested_mean, povline, z_min, z_max, ppp)
  names(results1) <- list("mean", "poverty_line", "z_min", "z_max", "ppp")

  # STEP 3: Estimate poverty measures based on identified parameters
  results2 <- prod_gd_estimate_lb(requested_mean, povline, p0, A, B, C)

  # STEP 4: Compute measure of regression fit
  results_fit <- gd_compute_fit_lb(welfare, population, results2$headcount, A, B, C)

  res <- c(results1, results2, results_fit, reg_results)

  return(res)

}

#' Estimates poverty and inequality stats from beta Lorenz fit
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
  pov_stats <- gd_compute_poverty_stats_lb(mean, povline, A, B, C)

  # Check validity
  validity <- check_curve_validity_lb(headcount = pov_stats[["headcount"]], A, B, C)

  out <- list(headcount = pov_stats$headcount,
              poverty_gap = pov_stats$pg,
              poverty_severity = pov_stats$p2,
              eh = pov_stats$eh,
              epg = pov_stats$epg,
              ep = pov_stats$ep,
              gh = pov_stats$gh,
              gpg = pov_stats$gpg,
              gp = pov_stats$gp,
              watts = pov_stats$watts,
              dl = pov_stats$dl,
              ddl = pov_stats$ddl,
              is_normal = validity$is_normal,
              is_valid = validity$is_valid)

  return(out)

}
