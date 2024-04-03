#' Create synthetic vector based on Lorenz  parameters of group data
#'
#' Compute distributional statistics for grouped data by selecting the best
#' functional fit for the Lorenz curve (either beta or quadratic).
#'
#' @inheritParams gd_compute_pip_stats
#' @inheritParams gd_compute_dist_stats_lb
#' @param nobs numeric: Number of observations to be used in synthetic vector
#' @param selected_model character or NULL: force to use one model, either
#'   quadratic (q) or Beta (b). If NULL (default), the methodological protocol
#'   will select the model. This argument is only useful for testing purposes.
#'
#' @return list
#' @keywords internal
#' @examples
#' # Create synthetic dataset
#' res <- wbpip:::sd_create_synth_vector(
#'   welfare = grouped_data_ex2$welfare,
#'   population = grouped_data_ex2$weight,
#'   mean = 8,
#'   pop = NULL,
#'   p0 = 0.5,
#'   nobs = 1e5)
#' str(res)
#'
sd_create_synth_vector <- function(welfare,
                                   population,
                                   mean,
                                   pop            = NULL,
                                   p0             = 0.5,
                                   nobs           = 1e5,
                                   selected_model = NULL,
                                   verbose        = FALSE) {


  # Check arguments
  if (!is.null(selected_model) && !selected_model  %in% c("q", "b") ) {
      cli::cli_abort(c("Incorrect selected model",
                       "must be either {.field 'q'} or {.field 'b'},
                       not {.val {selected_model}}"))
  }


  # Apply Lorenz quadratic fit ----------------------------------------------

  ## STEP 1: Prep data to fit functional form-------------
  prepped_data <- create_functional_form_lq(
    welfare = welfare, population = population
  )

  ## STEP 2: Estimate regression coefficients using LQ parameterization------
  reg_results_lq <- regres(prepped_data, is_lq = TRUE)
  reg_coef_lq <- reg_results_lq$coef

  kv <- gd_lq_key_values(A = reg_coef_lq[1],
                         B = reg_coef_lq[2],
                         C = reg_coef_lq[3])

  ## STEP 3: Calculate distributional stats
  results_lq <- gd_estimate_dist_stats_lq(
    mean = mean, p0 = p0, A = reg_coef_lq[1],
    B = reg_coef_lq[2], C = reg_coef_lq[3],
    key_values = kv
  )

  results_lq <- append(results_lq, reg_results_lq)

  # Apply Lorenz beta fit ---------------------------------------------------

  ## STEP 1: Prep data to fit functional form --------------
  prepped_data <- create_functional_form_lb(
    welfare = welfare, population = population
  )

  ## STEP 2: Estimate regression coefficients using LB parameterization
  reg_results_lb <- regres(prepped_data, is_lq = FALSE)
  reg_coef_lb <- reg_results_lb$coef

  ## STEP 3: Calculate distributional stats ---------------
  results_lb <- gd_estimate_dist_stats_lb(
    mean = mean, p0 = p0, A = reg_coef_lb[1],
    B = reg_coef_lb[2], C = reg_coef_lb[3]
  )

  results_lb <- append(results_lb, reg_results_lb)


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # create template for synthetic vector   ---------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## welfare --------
  # Create equally spaced distribution points that will be used to compute
  # the vector to welfare values
  first <- 1 / (2 * nobs)
  last <- 1 - (1 / (2 * nobs))
  n <- c(1:nobs)
  weight_range <- (n - 1) / (nobs - 1) * ((last) - (first)) + (first)


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Apply selection rules   ---------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # 3 Selection of Lorenz fit for distributional statistics ----
  if (is.null(selected_model)) {

    use_lq_for_dist <- use_lq_for_distributional(
      lq = results_lq,
      lb = results_lb
    )

  } else {
    if (selected_model == "q") {
      use_lq_for_dist <- TRUE
    } else {
      use_lq_for_dist <- FALSE
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## apply correct lorenz --------
  if (use_lq_for_dist) {
    A <- reg_coef_lq[1]
    B <- reg_coef_lq[2]
    C <- reg_coef_lq[3]

    # Compute welfare values

    # Vectorize is faster than purrr
    # vderive_lq <- Vectorize(derive_lq, vectorize.args = "x")
    # welfare_s <- vderive_lq(weight_range, A, B, C) * mean

    welfare_s <- derive_lq(weight_range, A, B, C, key_values = kv) * mean

    model_used <- "quadratic Lorenz"

  } else {
    A <- reg_coef_lb[1]
    B <- reg_coef_lb[2]
    C <- reg_coef_lb[3]

    # Compute welfare values

    # vderive_lb <- Vectorize(derive_lb, vectorize.args = "x")
    # welfare_s <- vderive_lb(weight_range, A, B, C) * mean

    welfare_s <- derive_lb(weight_range, A, B, C) * mean

    model_used <- "Beta Lorenz"
  }

  if (verbose) {
    cli::cli_alert("Parameters used in {.field {model_used}} model")
    cli::cli_dl(c(A = A, B = B, C = C, mean = mean))
  }

  # manage population
  if (is.null(pop)) {
    weight <- 1
  } else {
    weight <- pop / nobs
  }

  df <- data.table::data.table(
    welfare = welfare_s,
    weight  = weight
  )

  return(df)
}
