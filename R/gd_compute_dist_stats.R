#' Computes distributional statistics (grouped)
#'
#' Compute distributional statistics for grouped data by selecting the best
#' functional fit for the Lorenz curve (either beta or quadratic).
#'
#' @inheritParams gd_compute_pip_stats
#' @return list
#' @keywords internal
gd_compute_dist_stats <- function(welfare,
                                  population,
                                  mean,
                                  p0 = 0.5) {


  # Apply Lorenz quadratic fit ----------------------------------------------

  # STEP 1: Prep data to fit functional form
  prepped_data <- create_functional_form_lq(
    welfare = welfare, population = population)

  # STEP 2: Estimate regression coefficients using LQ parameterization
  reg_results_lq <- regres(prepped_data, is_lq = TRUE)
  reg_coef_lq <- reg_results_lq$coef

  # STEP 3: Calculate distributional stats
  results_lq <- gd_estimate_dist_stats_lq(
    mean = mean,  p0 = p0, A = reg_coef_lq[1],
    B = reg_coef_lq[2], C = reg_coef_lq[3])

  results_lq <- append(results_lq, reg_results_lq)

  # Apply Lorenz beta fit ---------------------------------------------------

  # STEP 1: Prep data to fit functional form
  prepped_data <- create_functional_form_lb(
    welfare = welfare, population = population)

  # STEP 2: Estimate regression coefficients using LB parameterization
  reg_results_lb <- regres(prepped_data, is_lq = FALSE)
  reg_coef_lb <- reg_results_lb$coef

  # STEP 3: Calculate distributional stats
  results_lb <- gd_estimate_dist_stats_lb(
    mean = mean, p0 = p0, A = reg_coef_lb[1],
    B = reg_coef_lb[2], C = reg_coef_lb[3])

  results_lb <- append(results_lb, reg_results_lb)

  # Apply selection rules -----------------------------------------------

  # STEP 4: Select best fit
  out <- gd_select_lorenz_dist(
    lq = results_lq, lb = results_lb)

  # Return only subset of variables
  out <- out[c("mean",
               "median",
               "gini",
               "mld",
               "polarization",
               "deciles")]

  return(out)
}


#' gd_estimate_dist_stats_lq
#' Estimates distributional stats from Quadratic Lorenz fit
#' @inheritParams gd_estimate_lq
#' @return list
#' @keywords internal
gd_estimate_dist_stats_lq <- function(mean, p0, A, B, C) {

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

  # Compute distributional measures -----------------------------------------

  dist_stats <- gd_compute_dist_stats_lq(mean, p0, A, B, C, e, m, n, r)

  out <- list(
    mean = mean,
    gini = dist_stats$gini,
    median = dist_stats$median,
    rmhalf = dist_stats$rmhalf,
    polarization = dist_stats$polarization,
    ris = dist_stats$ris,
    mld = dist_stats$mld,
    dcm = dist_stats$dcm,
    deciles = dist_stats$deciles,
    #is_normal = validity$is_normal,
    is_valid = validity$is_valid)

  return(out)

}

#' Estimates poverty and inequality stats from beta Lorenz fit
#' @inheritParams gd_estimate_lb
#' @return list
#' @keywords internal
gd_estimate_dist_stats_lb <- function(mean, p0, A, B, C) {

  # Compute distributional measures
  dist_stats <-
    gd_compute_dist_stats_lb(mean, p0, A, B, C)

  # Check validity
  validity <- check_curve_validity_dist_lb(A, B, C)

  out <- list(
    gini = dist_stats$gini,
    median = dist_stats$median,
    rmhalf = dist_stats$rmhalf,
    polarization = dist_stats$polarization,
    ris = dist_stats$ris,
    mld = dist_stats$mld,
    dcm = dist_stats$dcm,
    deciles = dist_stats$deciles,
    is_normal = validity$is_normal,
    is_valid = validity$is_valid)

  return(out)

}

#' gd_select_lorenz_dist
#' Select best Lorenz fit for distributional statistics
#' @inheritParams gd_select_lorenz
#' @return list
#' @keywords internal
gd_select_lorenz_dist <- function(lq, lb) {

  # Set default value
  datamean <- lq[["mean"]]
  is_valid <- lq[["is_valid"]] | lb[["is_valid"]]
  # is_normal <- lq[["is_normal"]] | lb[["is_normal"]]

  # Selection of Lorenz fit for distributional statistics
  use_lq_for_dist <-
    use_lq_for_distributional(lq = lq, lb = lb)

  # Retrieve distributional statistics
  dist <-
    retrieve_distributional(
      lq = lq,
      lb = lb,
      is_valid = is_valid,
      use_lq_for_dist = use_lq_for_dist)

  return(list(
    mean             = datamean,
    z_min            = dist[["z_min"]],
    z_max            = dist[["z_max"]],
    gini             = dist[["gini"]],
    median           = dist[["median"]],
    rmhalf           = dist[["rmhalf"]],
    polarization     = dist[["polarization"]],
    ris              = dist[["ris"]],
    mld              = dist[["mld"]],
    deciles          = dist[["deciles"]],
    sse              = dist[["sse"]]
  ))

}


#' check_curve_validity_dist_lb
#' Check validity of Lorenz beta fit for distributional statistics
#' @inheritParams check_curve_validity_lb
#' @return list
#' @keywords internal
check_curve_validity_dist_lb <- function(A, B, C) {

  is_valid <- TRUE

  for (w in seq(from = 0.001, to = 0.1, by = 0.05)) {
    if (derive_lb(w, A, B, C) < 0) {
      is_valid <- FALSE
      break
    }
  }

  if (is_valid) {
    for (w in seq(from = 0.001, to = 0.999, by = 0.05)) {
      if (DDLK(w, A, B, C) < 0) { # What does DDLK stands for?? What does it do?
        is_valid <- FALSE
        break
      }
    }
  }

  return(list(
    is_valid = is_valid)
  )

}
