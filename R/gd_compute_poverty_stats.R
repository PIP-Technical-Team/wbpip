#' Computes poverty statistics (grouped)
#'
#' Compute poverty statistics for grouped data by selecting the best functional
#' fit for the Lorenz curve (either beta or quadratic).
#'
#' @inheritParams gd_compute_pip_stats
#' @inheritParams gd_compute_poverty_stats_lb
#'
#' @return list
#' @keywords internal
gd_compute_poverty_stats <- function(welfare,
                                     povline,
                                     population,
                                     requested_mean,
                                     popshare = NULL,
                                     default_ppp = NULL,
                                     ppp = NULL,
                                     p0 = 0.5) {


  # Apply Lorenz quadratic fit ----------------------------------------------
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
  reg_results_lq <- regres(prepped_data, is_lq = TRUE)
  reg_coef_lq <- reg_results_lq$coef

  # STEP 3: Calculate poverty stats
  results_lq <- gd_estimate_poverty_stats_lq(
    mean = requested_mean,
    povline = povline,
    A = reg_coef_lq[1],
    B = reg_coef_lq[2],
    C = reg_coef_lq[3])

  # STEP 4: Compute measure of regression fit
  fit_lq <- gd_compute_fit_lq(welfare = welfare,
                              population = population,
                              headcount = results_lq$headcount,
                              A = reg_coef_lq[1],
                              B = reg_coef_lq[2],
                              C = reg_coef_lq[3])

  results_lq <- c(results_lq, reg_results_lq, fit_lq)

  # Apply Lorenz beta fit ---------------------------------------------------

  # STEP 1: Prep data to fit functional form
  prepped_data <- create_functional_form_lb(
    welfare = welfare, population = population)

  # STEP 2: Estimate regression coefficients using LB parameterization
  reg_results_lb <- regres(prepped_data, is_lq = FALSE)
  reg_coef_lb <- reg_results_lb$coef

  # STEP 3: Calculate distributional stats
  results_lb <- gd_estimate_poverty_stats_lb(
    mean = requested_mean,
    povline = povline,
    A = reg_coef_lb[1],
    B = reg_coef_lb[2],
    C = reg_coef_lb[3])

  # STEP 4: Compute measure of regression fit
  fit_lb <- gd_compute_fit_lb(welfare = welfare,
                              population = population,
                              headcount = results_lq$headcount,
                              A = reg_coef_lb[1],
                              B = reg_coef_lb[2],
                              C = reg_coef_lb[3])

  results_lb <- c(results_lb, reg_results_lb, fit_lb)

  # Apply selection rules -----------------------------------------------

  # STEP 4: Select best fit
  out <- gd_select_lorenz_poverty(
    lq = results_lq,
    lb = results_lb)

  # Return only subset of variables
  out <- out[c("poverty_line",
               "headcount",
               "poverty_gap",
               "poverty_severity",
               "watts")]
  out$poverty_line <- povline

  return(out)
}


#' gd_estimate_poverty_stats_lq
#' Estimates poverty stats from Quadratic Lorenz fit
#' @inheritParams gd_estimate_lq
#' @return list
#' @keywords internal
gd_estimate_poverty_stats_lq <- function(mean, povline, A, B, C) {

  # Compute Lorenz quadratic  -----------------------------------------------

  # Compute key numbers from Lorenz quadratic form
  # Theorem 3 from original Lorenz quadratic paper
  e <- -(A + B + C + 1) # e = -(A + B + C + 1): condition for the curve to go through (1, 1)
  m <- (B^2) - (4 * A) # m < 0: condition for the curve to be an ellipse (m is called alpha in paper)
  n <- (2 * B * e) - (4 * C) # n is called Beta in paper
  r <- (n^2) - (4 * m * e^2) # r is called K in paper

  validity <- check_curve_validity_lq(A, B, C, e, m, n, r)

  r <- sqrt(r)
  s1 <- (r - n) / (2 * m)
  s2 <- -(r + n) / (2 * m)

  # Compute poverty measures -----------------------------------------

  pov_stats <- gd_compute_poverty_stats_lq(mean = mean,
                                           povline = povline,
                                           A = A,
                                           B = B,
                                           C = C,
                                           e = e,
                                           m = m,
                                           n = n,
                                           r = r,
                                           s1 = s1,
                                           s2 = s2)

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

#' Estimates poverty stats from beta Lorenz fit
#' @inheritParams gd_estimate_lb
#' @return list
#' @keywords internal
gd_estimate_poverty_stats_lb <- function(mean, povline, A, B, C) {

  # Compute distributional measures
  pov_stats <-
    gd_compute_poverty_stats_lb(mean = mean,
                                povline = povline,
                                A = A,
                                B = B,
                                C = C)

  # Check validity
  validity <- check_curve_validity_lb(headcount = pov_stats$headcount, A, B, C)

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

#' gd_select_lorenz_dist
#' Select best Lorenz fit for poverty statistics
#' @inheritParams gd_select_lorenz
#' @return list
#' @keywords internal
gd_select_lorenz_poverty <- function(lq, lb) {

  # Set default value
  datamean <- lq[["mean"]]
  is_normal <- lq[["is_normal"]] | lb[["is_normal"]]

  # Selection of Lorenz fit for poverty statistics
  use_lq_for_pov <- use_lq_for_poverty(lq = lq,
                                       lb = lb)

  # Retrieve poverty statistics
  pov <- retrieve_poverty(lq = lq,
                          lb = lb,
                          is_normal = is_normal,
                          use_lq_for_pov = use_lq_for_pov)

  return(list(
    mean             = datamean,
    poverty_line     = pov[["poverty_line"]],
    headcount        = pov[["headcount"]],
    poverty_gap      = pov[["poverty_gap"]],
    poverty_severity = pov[["poverty_severity"]],
    eh               = pov[["eh"]],
    epg              = pov[["epg"]],
    ep               = pov[["ep"]],
    gh               = pov[["gh"]],
    gpg              = pov[["gpg"]],
    gp               = pov[["gp"]],
    watts            = pov[["watts"]]
  ))

}


