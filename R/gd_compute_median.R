#' calculate the median for group data
#'
#' @inheritParams gd_compute_dist_stats
#'
#' @return scalar
#' @keywords internal
gd_compute_median <- function(welfare,
                              population,
                              mean,
                              p0 = 0.5) {


  # Apply Lorenz quadratic fit ----------------------------------------------

  # STEP 1: Prep data to fit functional form
  prepped_data <- create_functional_form_lq(
    welfare    = welfare,
    population = population)

  # STEP 2: Estimate regression coefficients using LQ parameterization
  reg_results_lq <- regres(prepped_data, is_lq = TRUE)
  reg_coef_lq    <- reg_results_lq$coef

  # STEP 3: Calculate distributional stats
  results_lq <- gd_estimate_dist_stats_lq(
    mean = mean,
    p0   = p0,
    A    = reg_coef_lq[1],
    B    = reg_coef_lq[2],
    C    = reg_coef_lq[3])

  lq <- append(results_lq, reg_results_lq)

  # Apply Lorenz beta fit ---------------------------------------------------

  # STEP 1: Prep data to fit functional form
  prepped_data <- create_functional_form_lb(
    welfare    = welfare,
    population = population)

  # STEP 2: Estimate regression coefficients using LB parameterization
  reg_results_lb <- regres(prepped_data, is_lq = FALSE)
  reg_coef_lb    <- reg_results_lb$coef

  # STEP 3: Calculate distributional stats
  results_lb <- gd_estimate_dist_stats_lb(
    mean = mean,
    p0   = p0,
    A    = reg_coef_lb[1],
    B    = reg_coef_lb[2],
    C    = reg_coef_lb[3])

  lb <- append(results_lb, reg_results_lb)

  # Apply selection rules -----------------------------------------------
  is_valid <- lq[["is_valid"]] | lb[["is_valid"]]
  use_lq_for_dist <-
    use_lq_for_distributional(lq = lq, lb = lb)


  if (is_valid) {
    if (use_lq_for_dist) {

      median <- lq[["median"]]

    } else {

      median <- lb[["median"]]

    }

  } else {
    cli::cli_alert_danger("Group data is invalid. Returning NA")
    median <- NA
  }

  return(median)
}
