#' @import purrr
#' @import assertthat
NULL

#' Fill gaps
#'
#' Calculate poverty statistics for a common request year by applying the World
#' Bank official method for interpolating and extrapolating surveys.
#'
#' @param request_year integer: A value with the request year.
#' @param data list: A list with one or two data frames containing survey data.
#' @param survey_mean numeric: A vector with one or two survey means.
#' @param survey_year numeric: A vector with one or two survey years.
#' @param proxy list: A list with proxy values to be used to estimate the growth
#'   rate.
#' @param data_type character: Type of data, either micro or grouped.
#' @param poverty_line numeric: Daily poverty line in international dollars.
#'
#' @examples
#'
#' @export
fill_gaps <- function(request_year,
                      data = list(df0, df1 = NULL),
                      survey_mean,
                      survey_year,
                      proxy = list(value0, value1 = NULL, req_value),
                      data_type = c('microdata', 'groupdata'),
                      poverty_line = 1.9) {

  # CHECKS
  environment(check_inputs_fill_gaps) <- environment()
  check_inputs_fill_gaps()

  # Match arguments
  data_type <- match.arg(data_type)

  # Adjust proxy values for potential decimal survey years.
  proxy <- fg_adjust_decimal(survey_year, proxy)

  # Calculate request year mean
  request_year_mean <- fg_calculate_request_mean(survey_mean, proxy)

  # Calculate poverty stats
  if (data_type == 'microdata') {
    if (length(request_year_mean) == 1) {

      weights0 <- fg_get_weights(data$df0)

      # Calculate poverty statistics for the request year
      out <- md_compute_pip_stats(welfare = data$df0$welfare,
                                  weight = weights0,
                                  povline = poverty_line,
                                  default_ppp = 1,
                                  requested_mean = request_year_mean)

    } else {

      weights0 <- fg_get_weights(data$df0)
      weights1 <- fg_get_weights(data$df1)

      # Calculate statistics for the first survey year
      dl0 <- md_compute_pip_stats(welfare = data$df0$welfare,
                                  weight = weights0,
                                  povline = poverty_line,
                                  default_ppp = 1,
                                  requested_mean = request_year_mean[1])

      # Calculate statistics for the second survey year
      dl1 <- md_compute_pip_stats(welfare = data$df1$welfare,
                                  weight = weights1,
                                  povline = poverty_line,
                                  default_ppp = 1,
                                  requested_mean = request_year_mean[2])

      # Calculate poverty statistics for the request year (weighted average)
      out <- fg_adjust_poverty_stats(dl0, dl1, survey_year, request_year)
    }


  } else if (data_type == 'groupdata') {

    # GROUPED DATA
    # out <- gd_compute_pip_stats()

  }
  # } else if () {
  #
  #   # AGGREGATED data
  #
  # }

  return(out)

}

#' fg_extrapolate_survey_mean
#'
#' @param survey_mean numeric: A vector with survey means.
#' @param proxy list: A list with proxy values.
#' @return numeric
#' @noRd
fg_extrapolate_survey_mean <- function(survey_mean, proxy) {
  # Growth factor = request_value / value
  growth_factor <-
    purrr::map_dbl(c(proxy$value0, proxy$value1), .f = function(x, y) {y / x},
                   y = proxy$req_value)
  # Extrapolated value = survey_mean * growth factor
  out <- purrr::map2_dbl(survey_mean, growth_factor,
                         .f = function(x, y) {x * y})
  return(out)
}

#' fg_interpolate_survey_mean
#'
#' Interpolate survey means based on growth
#'
#' @param survey_mean numeric: A vector with survey means.
#' @param proxy list: A list with proxy values.
#' @return numeric
#' @noRd
fg_interpolate_survey_mean <- function(survey_mean, proxy) {

  # Growth factor = (req_value - value0) / (value1 - value0)
  growth_factor <- (proxy$req_value - proxy$value0) / (proxy$value1 - proxy$value0)

  # Interpolated value = growth factor * (mean1 - mean0) + mean0
  out <- growth_factor * (survey_mean[2] - survey_mean[1]) + survey_mean[1]

  return(out)
}

#' fg_calculate_request_mean
#'
#' @param survey_mean numeric: A vector with one or two survey means.
#' @param proxy list: A list with proxy values.
#' @return numeric
#' @noRd
fg_calculate_request_mean <- function(survey_mean, proxy) {
  if (length(survey_mean) == 1) {
    if (fg_is_one_point_adjusted(survey_mean, proxy$value0, proxy$req_value)) {
      request_mean <- fg_extrapolate_survey_mean(survey_mean, proxy)
    } else {
      request_mean <- NA
    }
  } else {
    proxy_values <- c(proxy$value0, proxy$value1)
    if (fg_is_non_monotonic(survey_mean, proxy_values, proxy$req_value)) {
      request_mean <- fg_extrapolate_survey_mean(survey_mean, proxy)
    } else if (fg_is_same_direction_interpolated(survey_mean, proxy_values, proxy$req_value)) {
      request_mean <- fg_interpolate_survey_mean(survey_mean, proxy)
    } else {
      request_mean <- NA
    }
  }
  return(request_mean)
}

#' fg_adjust_poverty_stats
#'
#' Calculate a weighted average for poverty statistics based on the difference
#' between the two survey_years and the request year. This is used when the
#' interpolation method is non-monotonic.
#'
#' @param stats0 list: A list with poverty statistics.
#' @param stats1 list: A list with poverty statistics.
#' @param request_year integer: A value with the request year.
#' @param survey_year numeric: A vector with one or two survey years.
#' @return numeric
#' @noRd
fg_adjust_poverty_stats <- function(stats0, stats1, survey_year, request_year) {
  purrr::map2(stats0, stats1,
              .f = function(measure0, measure1, survey_year, request_year) {
                ((survey_year[2] - request_year) * measure0 +
                   (request_year - survey_year[1]) * measure1) /
                  (survey_year[2] - survey_year[1])
              }, survey_year, request_year)

}

#' fg_adjust_decimal
#' @param survey_year numeric: A vector with one or two survey means.
#' @param x list: A list with proxy values.
#' @return numeric: A vector with one or two survey years.
#' @noRd
fg_adjust_decimal <- function(survey_year, x) {
  x$value0 <- get_decimal_year_value(survey_year[1], x$value0)
  if (!is.null(x$value1)) {
    x$value1 <- get_decimal_year_value(survey_year[2], x$value1)
  }
  return(x)
}
#' fg_get_weights
#' @param df data.frame: A data frame with a welfare column.
#' @return numeric:
#' @noRd
fg_get_weights <- function(df) {
  if ('weight' %in% colnames(df)) {
    weights <- df$weight
  } else {
    weights <- rep(1, length(df$welfare))
  }
  return(weights)
}

#' is_monotonic
#' @param x1 numeric: Value for the first year.
#' @param x2 numeric: Value for the second year.
#' @param r numeric: Value for the request year.
#' @return logical
#' @noRd
is_monotonic <- function(x1, x2, r) {((r - x1) * (x2 - r)) > 0}

#' is_same_direction
#' @param x numeric: A vector with values to compare.
#' @param y numeric: A vector with values to compare.
#' @return logical
#' @noRd
is_same_direction <- function(x, y) {(x[2] - x[1]) * (y[2] - y[1]) > 0}

#' fg_is_non_monotonic
#' @param survey_mean numeric: A vector with one or two survey means.
#' @return logical
#' @noRd
fg_is_non_monotonic <- function(survey_mean, proxy_value, req_value) {

  # CHECKS
  if (anyNA(proxy_value) | anyNA(req_value)) return(FALSE)
  if (length(survey_mean) == 1) return(FALSE)

  if (is_monotonic(x1 = proxy_value[1], x2 = proxy_value[2], r = req_value)) {
    if (!is_same_direction(x = proxy_value, y = survey_mean)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(TRUE)
  }
}

#' fg_is_same_direction_interpolated
#' @param survey_mean numeric: A vector with one or two survey means.
#' @return logical
#' @noRd
fg_is_same_direction_interpolated <- function(survey_mean, proxy_value, req_value) {

  # CHECKS
  if (anyNA(proxy_value) | anyNA(req_value)) return(FALSE)
  if (length(survey_mean) == 1) return(FALSE)

  if (is_monotonic(x1 = proxy_value[1], x2 = proxy_value[2], r = req_value)) {
    if (is_same_direction(x = proxy_value, y = survey_mean)) {
      return(TRUE)
    }
  } else {
    return(FALSE)
  }
}

#' fg_is_one_point_adjusted
#' @param survey_mean numeric: A vector with one or two survey means.
#' @return numeric
#' @noRd
fg_is_one_point_adjusted <- function(survey_mean, proxy_value, req_value){

  # CHECKS
  if (anyNA(proxy_value) | anyNA(req_value)) return(FALSE)

  if (length(survey_mean) == 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' check_inputs_fill_gaps
#' @return logical
#' @noRd
check_inputs_fill_gaps <- function() {

  # request_year
  assertthat::assert_that(is.numeric(request_year))
  assertthat::assert_that(!length(request_year) > 1,
                          msg = paste0(
                            'You can only interpolate or extrapolate to one request year ',
                            'at a time.'))

  # welfare
  assertthat::assert_that('welfare' %in% colnames(data$df0))
  assertthat::assert_that(is.numeric(data$df0$welfare))
  if (!is.null(data$df1)) assertthat::assert_that('welfare' %in% colnames(data$df1))
  if (!is.null(data$df1)) assertthat::assert_that(is.numeric(data$df1$welfare))

  # survey_mean
  if (!is.null(survey_mean)) assertthat::assert_that(is.numeric(survey_mean))
  assertthat::assert_that(!length(survey_mean) > 2,
                          msg = 'You can\'t interpolate between more than to surveys.')

  # survey_year
  assertthat::assert_that(is.numeric(survey_year))
  assertthat::assert_that(!length(survey_year) > 2,
                          msg = 'You can\'t interpolate between more than to surveys.')

  # proxy
  assertthat::assert_that(is.numeric(proxy$value0))
  if (!is.null(proxy$value1)) assertthat::assert_that(is.numeric(proxy$value1))
  assertthat::assert_that(is.numeric(proxy$req_value))

  # proxy vs survey year
  if (get_weights(survey_year[1])[1] != 1)
    assertthat::assert_that(length(proxy$value0) == 2,
                            msg = 'You must supply two calendar year values,
                            since you supplied a decimal survey year.')
  if (length(survey_year) == 2) {
    if (get_weights(survey_year[2])[1] != 1)
      assertthat::assert_that(length(proxy$value1) == 2,
                              msg = 'You must supply two calendar year values,
                              since you supplied a decimal survey year.')
  }

  # poverty_line
  assertthat::assert_that(is.numeric(poverty_line))

}
