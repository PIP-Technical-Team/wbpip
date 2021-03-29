#' Computes poverty statistics (Lorenz quadratic)
#' Version used in production
#'
#' Compute poverty statistics for grouped data using the quadratic functional
#' form of the Lorenz qurve.
#'
#' @inheritParams gd_compute_pip_stats
#'
#' @examples
#' # Set initial parameters
#' L <- c(0.00208, 0.01013, 0.03122, 0.07083, 0.12808, 0.23498, 0.34887,
#'   0.51994, 0.6427, 0.79201, 0.86966, 0.91277, 1)
#' P <- c(0.0092, 0.0339, 0.085, 0.164, 0.2609, 0.4133, 0.5497, 0.7196,
#'   0.8196, 0.9174, 0.957, 0.9751, 1)
#' mu  <- 109.9 # mean
#' z   <- 89    # poverty line
#'
#' res <- wbpip:::prod_gd_compute_pip_stats_lq(
#'   welfare = L,
#'   population = P,
#'   requested_mean = mu,
#'   povline = z)
#' res$headcount
#'
#' res2 <- wbpip:::prod_gd_compute_pip_stats_lq(
#'   welfare = L,
#'   population = P,
#'   requested_mean = mu,
#'   popshare = res$headcount)
#' res2$povline
#'
#' @return list
#' @keywords internal
prod_gd_compute_pip_stats_lq <- function(welfare,
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
  prepped_data <- create_functional_form_lq(welfare = welfare,
                                            population = population)

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
  z_min <- ifelse(z_min < 0, 0, z_min)

  results1 <- list(requested_mean, povline, z_min, z_max, ppp)
  names(results1) <- list("mean", "poverty_line", "z_min", "z_max", "ppp")

  # STEP 3: Estimate poverty measures based on identified parameters
  results2 <- prod_gd_estimate_lq(requested_mean, povline, p0, A, B, C)

  # STEP 4: Compute measure of regression fit
  results_fit <- gd_compute_fit_lq(welfare, population, results2$headcount, A, B, C)

  res <- c(results1, results2, results_fit, reg_results)

  return(res)

}

#' Estimates poverty and inequality stats from Quadratic Lorenz fit
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
prod_gd_estimate_lq <- function(mean, povline, p0, A, B, C) {

  # Compute key numbers from Lorenz quadratic form
  # Theorem 3 from original Lorenz quadratic paper
  e <- -(A + B + C + 1) # e = -(A + B + C + 1): condition for the curve to go through (1, 1)
  m <- (B^2) - (4 * A) # m < 0: condition for the curve to be an ellipse (m is called alpha in paper)
  n <- (2 * B * e) - (4 * C) # n is called Beta in paper
  r <- (n^2) - (4 * m * e^2) # r is called K in paper

  validity <- check_curve_validity_lq(A, B, C, e, m, n, r)
  if (validity$is_valid == FALSE & validity$is_normal == FALSE) {
    return(empty_gd_compute_pip_stats_response)
  }

  r <- sqrt(r)
  s1 <- (r - n) / (2 * m)
  s2 <- -(r + n) / (2 * m)

  # Compute poverty stats ---------------------------------------------------

  pov_stats <- gd_compute_poverty_stats_lq(mean, povline, A, B, C, e, m, n, r, s1, s2)

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
