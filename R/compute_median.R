#' compute median for different methods
#'
#' @param dt data frame with either micro data, group data, or imputed data
#' @param welfare welfare vector
#' @param weight population weights vector
#'
#' @return
#' @export
#'
#' @examples
compute_median <- function(dt, ...) {
  UseMethod("compute_median", dt)
}

#' @export
compute_median.default <- function(dt, welfare, weight, ...) {

  if (!is.data.table(dt)) {
    dt <- as.data.table(dt)
  }

  compute_median_md(dt, welfare, weight)
}

#' compute median for microdata
#'
#' @param dt dataframe with microdata
#' @param level character: data level to filter data. It assumes variable
#'   `max_domain` in data set
#' @inheritParams compute_median
#' @return scalar
#' @export
compute_median.pipmd <- function(dt, welfare, weight, level = NULL) {

  # Get data info
  dti <- get_dt_info(dt)

  # get estimates by level
  p50  <- purrr::map(.x = dti$pop_level,
                     .f = ~compute_median_md(dt, welfare, weight, level = .x))

  names(p50) <- dti$pop_level

  if (dti$data_level != "D1") { # Urban/rural or subnat level

    # national mean
    p50_national <- compute_median_md(dt, welfare, weight)

    p50 <- append(list(p50_national), p50)
    names(p50) <- c("national", dti$pop_level)

  }

  return(p50)
}


#' calculate the median for group data
#'
#' @param  dt dataframe with group data
#' @param level character: data level to filter data. It assumes variable
#'   `max_domain` in data set
#' @param mean numeric. Named vector of welfare mean for each data level
#'   available in data frame `dt`
#' @inheritParams gd_compute_dist_stats
#' @inheritParams compute_median
#'
#' @return scalar
#' @export
compute_median.pipgd <- function(dt,
                              welfare,
                              weight,
                              mean,
                              p0 = 0.5,
                              pop = NULL) {
  # Get data info
  dti <- get_dt_info(dt)

  # get estimates by level
  p50  <- purrr::map(.x = dti$pop_level,
                     .f = ~compute_median_gd(dt,  welfare, weight, mean, level = .x))

  names(p50) <- dti$pop_level

  if (dti$data_level != "D1") { # Urban/rural or subnat level

    if (is.null(pop)) {
      msg     <- "Population data frame `pop` required when data level of
      grouped data is different from national"

      hint    <- "make sure you include population data frame using
      `pipload::pip_load_aux('pop')`"

      rlang::abort(c(
                    msg,
                    i = hint
                    ),
                    class = "wbpip_error"
                    )
    }

    # create synthetic vector
    wf <- purrr::map_df(.x = dti$pop_level,
                          .f = ~get_synth_vector(dt, pop, mean, level = .x))

    # national mean
    p50_national <- compute_median_md(wf, "welfare", "weight")

    p50 <- append(list(p50_national), p50)
    names(p50) <- c("national", dti$pop_level)

  }

  return(p50)

}

#' compute median for microdata
#'
#' @param dt dataframe with microdata
#' @inheritParams compute_median
#' @return scalar
#' @export
compute_median_md <- function(dt, welfare, weight, level = NULL) {

  if (!is.null(level)) {
    dt  <- dt[max_domain == level]
  }

  wlf <- dt[[welfare]]
  wgt <- dt[[weight]]

  median <- collapse::fmedian(x = wlf,
                              w = wgt)
  return(median)
}

#' calculate the median for group data
#'
#' @param  dt dataframe with group data
#' @inheritParams gd_compute_dist_stats
#' @inheritParams compute_median
#'
#' @return scalar
#' @export
compute_median_gd <- function(dt,
                              welfare,
                              weight,
                              mean,
                              p0 = 0.5,
                              level = NULL) {

  if (!is.null(level)) {
    dt   <- dt[max_domain == level]
    mean <- mean[level]
  }

  welfare    <- dt[[welfare]]
  population <- dt[[weight]]

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


#' @keywords internal
get_dt_info <- function(dt) {


  # Order by population data level
  data.table::setorder(dt, pop_data_level)
  cache_id <-  unique(dt$cache_id)

  return(list(cache_id = cache_id,
              # identify procedure
              source      = gsub("(.*_)([A-Z]+$)", "\\2", cache_id),
              data_level  = gsub("(.*_)(D[123])(.+$)", "\\2", cache_id),

              # NOTE: we should change variable pop_data_level to something more general. We could
              # use something similar to variable max_domain in the function db_filter_inventory
              pop_level = unique(dt$pop_data_level)
              ))

}














