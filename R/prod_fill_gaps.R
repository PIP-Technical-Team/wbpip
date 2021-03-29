#' Compute interpolated stats
#' Version used in production
#'
#' A helper function for calculating interpolated or extrapolated poverty
#' statistics.
#'
#' The predicted request year mean(s) must be in comparable international
#' dollars and adjusted for differences in purchasing-power, and changes in
#' prices and currencies.
#'
#' The survey data must contain a column named *welfare* and optionally a column
#' named *weight* if welfare values are to be weighted.
#'
#' @param request_year integer: A value with the request year.
#' @param data list: A list with one or two data frames containing survey data.
#'   See details.
#' @param predicted_request_mean numeric: A vector with one or two predicted
#' survey means. See details.
#' @param svy_mean_lcu numeric: A vector with one or two survey means.
#' @param survey_year numeric: A vector with one or two survey years.
#' @param poverty_line numeric: Daily poverty line in international dollars.
#' @param default_ppp numeric: Default purchasing power parity.
#' @param ppp numeric: PPP request by user.
#' @param distribution_type character: A vector with the type of distribution,
#'   must be either micro, group, aggregate or imputed.
#'
#' @seealso [deflate_welfare_mean()] [predict_request_year_mean()]
#' @return data.frame
#' @noRd
#'
prod_fg_compute_pip_stats <- function(request_year,
                                      data,
                                      predicted_request_mean,
                                      svy_mean_lcu,
                                      survey_year,
                                      default_ppp,
                                      ppp,
                                      distribution_type,
                                      poverty_line) {

  # Set type
  type <- distribution_type
  if (length(type) == 1 & length(predicted_request_mean) == 2) {
    type <- rep(type, 2)
  }

  # Number of supplied surveys
  n_surveys <- length(type)

  # Create list of parameters
  params <- prod_fg_create_params(
    predicted_request_mean = predicted_request_mean,
    svy_mean_lcu = svy_mean_lcu,
    data = data,
    poverty_line = poverty_line,
    default_ppp = default_ppp,
    ppp = ppp
  )

  # Calculate poverty stats
  dl <- vector(mode = "list", length = n_surveys)
  for (i in seq_along(type)) {
    dl[[i]] <- do.call(prod_fg_select_compute_pip_stats[[type[i]]], params[[i]])
  }

  # If interpolating between two surveys then calculate
  # a weighted average for the request year
  if (n_surveys == 2) {
    out <- prod_fg_adjust_poverty_stats(dl[[1]], dl[[2]], survey_year, request_year)
    # Else returned the extrapolation for the request year as is
  } else {
    out <- dl[[1]]
  }

  return(out)
}

#' Select correct function for imputation
#' Version used in PROD
#'
#' A small wrapper function for picking the correct `compute_pip_stats()`
#' function depending on distribution type.
#'
#' @noRd
prod_fg_select_compute_pip_stats <- list(
 micro = function(...) prod_md_compute_pip_stats(...),
 group = function(...) gd_compute_pip_stats(...),
 imputed = function(...) prod_md_compute_pip_stats(...)
)

#' prod_fg_create_params
#'
#' Create parameters to be used in `prod_fg_compute_pip_stats()`.
#'
#' @inheritParams prod_fg_compute_pip_stats
#' @return list
#' @noRd
prod_fg_create_params <- function(predicted_request_mean,
                                  svy_mean_lcu,
                                  data,
                                  poverty_line,
                                  default_ppp,
                                  ppp ) {

  # If one survey
  if (length(predicted_request_mean) == 1) {
    params <- list(
      params0 = list(
        welfare        = data$df0$welfare,
        population     = data$df0$weight,
        povline        = poverty_line,
        default_ppp    = default_ppp[1],
        ppp            = ppp,
        requested_mean = predicted_request_mean[1],
        svy_mean_lcu   = svy_mean_lcu[1]
      )
    )
    # If two surveys (micro or grouped)
  } else {
    params <- list(
      params0 = list(
        welfare = data$df0$welfare,
        population = data$df0$weight,
        povline = poverty_line,
        default_ppp = default_ppp[1],
        ppp = ppp,
        requested_mean = predicted_request_mean[1],
        svy_mean_lcu   = svy_mean_lcu[1]
      ),
      params1 =  list(
        welfare = data$df1$welfare,
        population = data$df1$weight,
        povline = poverty_line,
        default_ppp = default_ppp[2],
        ppp = ppp,
        requested_mean = predicted_request_mean[2],
        svy_mean_lcu   = svy_mean_lcu[2]
      )
    )
  }

  return(params)

}

#' Calculate a weighted average for poverty statistics based on the difference
#' between the two survey_years and the request year. This is used when the
#' interpolation method is non-monotonic.
#'
#' Version used in production. Ignores distributional stats
#'
#' @param stats0 list: A list with poverty statistics.
#' @param stats1 list: A list with poverty statistics.
#' @param request_year integer: A value with the request year.
#' @param survey_year numeric: A vector with one or two survey years.
#' @return numeric
#' @noRd
prod_fg_adjust_poverty_stats <- function(stats0, stats1, survey_year, request_year) {

  # Calculate a weighted average for the poverty stats by taking the
  # difference between the two survey years and the request year
  out <-
    purrr::map2(
      stats0, stats1,
      .f = function(measure0, measure1, survey_year, request_year) {
        ((survey_year[2] - request_year) * measure0 +
           (request_year - survey_year[1]) * measure1) /
          (survey_year[2] - survey_year[1])
      }, survey_year, request_year)

  return(out)
}
