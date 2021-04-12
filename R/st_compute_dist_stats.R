#' Create synthetic vector based on Lorenz  parameters of group data
#'
#' Compute distributional statistics for grouped data by selecting the best
#' functional fit for the Lorenz curve (either beta or quadratic).
#'
#' @inheritParams gd_compute_pip_stats
#' @inheritParams gd_compute_dist_stats_lb
#' @param pop numeric: a sinble number with the population at data_level
#'
#' @return list
#' @keywords internal
st_create_synth_vector <- function(welfare,
                                  population,
                                  mean,
                                  pop,
                                  p0 = 0.5) {


  # Apply Lorenz quadratic fit ----------------------------------------------

  ## STEP 1: Prep data to fit functional form-------------
  prepped_data <- create_functional_form_lq(
    welfare = welfare, population = population)

  ## STEP 2: Estimate regression coefficients using LQ parametrization------
  reg_results_lq <- regres(prepped_data, is_lq = TRUE)
  reg_coef_lq <- reg_results_lq$coef

  ## STEP 3: Calculate distributional stats
  results_lq <- gd_estimate_dist_stats_lq(
    mean = mean,  p0 = p0, A = reg_coef_lq[1],
    B = reg_coef_lq[2], C = reg_coef_lq[3])

  results_lq <- append(results_lq, reg_results_lq)

  # Apply Lorenz beta fit ---------------------------------------------------

  ## STEP 1: Prep data to fit functional form --------------
  prepped_data <- create_functional_form_lb(
    welfare = welfare, population = population)

  ## STEP 2: Estimate regression coefficients using LB parametrization
  reg_results_lb <- regres(prepped_data, is_lq = FALSE)
  reg_coef_lb <- reg_results_lb$coef

  ## STEP 3: Calculate distributional stats ---------------
  results_lb <- gd_estimate_dist_stats_lb(
    mean = mean, p0 = p0, A = reg_coef_lb[1],
    B = reg_coef_lb[2], C = reg_coef_lb[3])

  results_lb <- append(results_lb, reg_results_lb)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # create template for synthetic vector   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## welfare --------

  nobs  <- 1e5
  first <- 1/(2*nobs)
	last  <- 1-(1/(2*nobs))
	n     <- c(1:nobs)
	F     <- (n-1)/(nobs-1)*((last)-(first))+(first)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Apply selection rules   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #3 Selection of Lorenz fit for distributional statistics ----
  use_lq_for_dist <- use_lq_for_distributional(lq = results_lq,
                                               lb = results_lb)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## apply correct lorenz --------
  if (use_lq_for_dist) {
    A = reg_coef_lq[1]
    B = reg_coef_lq[2]
    C = reg_coef_lq[3]

    welfare_s  <- mean * derive_lq(F, A, B, C)


  } else {
    A = reg_coef_lb[1]
    B = reg_coef_lb[2]
    C = reg_coef_lb[3]

    welfare_s  <- mean * derive_lb(F, A, B, C)
  }

  df <- data.table::data.table(
    welfare = welfare_s,
    weight  = pop/nobs
  )


  return(df)
}
