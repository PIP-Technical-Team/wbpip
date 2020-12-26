# Add global variables to avoid NSE notes in R CMD check
if (getRversion() >= '2.15.1')
  utils::globalVariables(
    c('value0', 'value0', 'req_value')
  )

#' Predict request year mean
#'
#' Predict a welfare mean for a request year for which survey data is not
#' available.
#'
#' The survey mean(s) must be in comparable international dollars and adjusted
#' for differences in purchasing-power, and changes in prices and currencies.
#'
#' You will also need to supply a set of `proxy` values to calculate the growth
#' rate for the distribution. Typically this is national accounts data, e.g. the
#' Gross Domestic Product (GDP) or Household Final Consumption Expenditure
#' (HFCE).
#'
#' In case the survey spans two years, and you are using a decimal survey year
#' to take this into account, you will need to supply two proxy values for the
#' corresponding survey year.
#'
#' @param survey_year numeric: A vector with one or two survey years.
#' @param survey_mean numeric: A vector with one or two survey means.
#' @param proxy list: A list with proxy values.
#' \itemize{
#'   \item `value0` numeric: The proxy value(s) for the first survey year.
#'   \item `value1` numeric: The proxy value(s) for the second survey year.
#'   \item `req_value` numeric: The proxy value for the request year.
#'}
#' @return numeric
#' @seealso [deflate_welfare_mean()] [fill_gaps()]
#' @references
#' Prydz, E. B., D. Jolliffe, C. Lakner, D. G. Mahler, P. Sangraula. 2019.
#' "[National Accounts Data used in Global Poverty Measurement](http://documents1.worldbank.org/curated/en/664751553100573765/pdf/135460-WP-PUBLIC-Disclosed-3-21-2019.pdf)".
#' Global Poverty Monitoring Technical Note 8.
#' World Bank, Washington, DC.
#' @examples
#' # Extrapolate a single survey
#' predict_request_year_mean(
#'   survey_year = 2005,
#'   survey_mean = 2.0,
#'   proxy = list(value0 = 1350, req_value = 1500))
#'
#' # Interpolate two surveys (monotonic)
#' predict_request_year_mean(
#'   survey_year = c(2000, 2005),
#'   survey_mean = c(2.0, 3.0),
#'   proxy = list(value0 = 1350, value1 = 1600, req_value = 1500))
#'
#' # Interpolate two surveys (non-monotonic)
#' predict_request_year_mean(
#'   survey_year = c(2000, 2005),
#'   survey_mean = c(2.0, 3.0),
#'   proxy = list(value0 = 1350, value1 = 1500, req_value = 1600))
#'
#' # Extrapolate a single survey (w/ decimal year)
#' predict_request_year_mean(
#'   survey_year = 2000.3,
#'   survey_mean = 2.0,
#'   proxy = list(value0 = c(1350, 1400), req_value = 1600))
#'
#' @export
predict_request_year_mean <- function(survey_year, survey_mean,
                                proxy = list(value0, value1 = NULL, req_value)) {
  # CHECKS
  check_inputs_predict_request_year_mean(survey_year, survey_mean, proxy)

  # Calculate weighted average for proxy values,
  # if a decimal survey year is supplied
  proxy <- adjust_decimal(survey_year, proxy)

  # Predict survey mean
  predict_request_year_mean <- compute_predicted_mean(survey_mean, proxy)
  return(predict_request_year_mean)
}

#' compute_predicted_mean
#' @param survey_mean numeric: A vector with one or two survey means.
#' @param proxy list: A list with proxy values.
#' @return numeric
#' @export
compute_predicted_mean <- function(survey_mean, proxy) {

  #--------- verify missing values ---------
  lsm <- length(survey_mean)

  if (lsm == 1)  {

    if (is.na(survey_mean)) {
      return(NA)
    }

  } else {
    if (sum(is.na(survey_mean)) == 0) {

      survey_mean <- survey_mean

    } else if (sum(is.na(survey_mean)) == 1) {

      survey_mean <- survey_mean[which(!is.na(survey_mean))]

    } else {

      return(NA)

    }
  }

  #--------- calculations ---------

  if (length(survey_mean) == 1) {
    if (is_one_point_adjusted(survey_mean, proxy$value0, proxy$req_value)) {
      pred_mean <- extrapolate_survey_mean(survey_mean, proxy)
    } else {
      pred_mean <- NA
    }
  } else {
    proxy_values <- c(proxy$value0, proxy$value1)
    if (is_non_monotonic(survey_mean, proxy_values, proxy$req_value)) {
      pred_mean <- extrapolate_survey_mean(survey_mean, proxy)
    } else if (is_same_direction_interpolated(survey_mean, proxy_values, proxy$req_value)) {
      pred_mean <- interpolate_survey_mean(survey_mean, proxy)
      pred_mean <- rep(pred_mean, length(survey_mean))
    } else {
      pred_mean <- NA
    }
  }
  return(pred_mean)
}

#' extrapolate_survey_mean
#'
#' Extrapolate or or two survey means based on an external growth factor.
#'
#' @param survey_mean numeric: A vector with survey means.
#' @param proxy list: A list with proxy values.
#' @return numeric
#' @noRd
extrapolate_survey_mean <- function(survey_mean, proxy) {
  # Growth factor = request_value / value
  growth_factor <-
    purrr::map_dbl(c(proxy$value0, proxy$value1), .f = function(x, y) {y / x},
                   y = proxy$req_value)
  # Extrapolated value = survey_mean * growth factor
  out <- purrr::map2_dbl(survey_mean, growth_factor,
                         .f = function(x, y) {x * y})
  return(out)
}

#' interpolate_survey_mean
#'
#' Interpolate two survey means based on the difference between the two means
#' and an external growth factor.
#'
#' @param survey_mean numeric: A vector with survey means.
#' @param proxy list: A list with proxy values.
#' @return numeric
#' @noRd
interpolate_survey_mean <- function(survey_mean, proxy) {

  # Growth factor = (request_value - value0) / (value1 - value0)
  growth_factor <- (proxy$req_value - proxy$value0) / (proxy$value1 - proxy$value0)

  # Interpolated value = growth factor * (mean1 - mean0) + mean0
  out <- growth_factor * (survey_mean[2] - survey_mean[1]) + survey_mean[1]

  return(out)
}

#' is_non_monotonic
#' @param survey_mean numeric: A vector with one or two survey means.
#' @param proxy_value numeric: A vector with one or two proxy values,
#'   corresponding to year of the survey means.
#' @param proxy_value numeric: Proxy value for the request year.
#' @return logical
#' @noRd
is_non_monotonic <- function(survey_mean, proxy_value, req_value) {

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

#' is_same_direction_interpolated
#' @param survey_mean numeric: A vector with one or two survey means.
#' @param proxy_value numeric: A vector with one or two proxy values,
#'   corresponding to year of the survey means.
#' @param proxy_value numeric: Proxy value for the request year.
#' @return logical
#' @noRd
is_same_direction_interpolated <- function(survey_mean, proxy_value, req_value) {

  # CHECKS
  if (anyNA(proxy_value) | anyNA(req_value)) return(FALSE)
  if (length(survey_mean) == 1) return(FALSE)

  if (is_monotonic(x1 = proxy_value[1], x2 = proxy_value[2], r = req_value)) {
    if (is_same_direction(x = proxy_value, y = survey_mean)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}

#' is_one_point_adjusted
#' @param survey_mean numeric: A vector with one or two survey means.
#' @param proxy_value numeric: A vector with one or two proxy values,
#'   corresponding to year of the survey means.
#' @param proxy_value numeric: Proxy value for the request year.
#' @return logical
#' @noRd
is_one_point_adjusted <- function(survey_mean, proxy_value, req_value){

  # CHECKS
  if (anyNA(proxy_value) | anyNA(req_value)) return(FALSE)

  if (length(survey_mean) == 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
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

#' adjust_decimal
#' @param survey_year numeric: A vector with one or two survey means.
#' @param x list: A list with proxy values.
#' @return numeric: A vector with one or two survey years.
#' @noRd
adjust_decimal <- function(survey_year, x) {
  x$value0 <- get_decimal_year_value(survey_year[1], x$value0)
  if (!is.null(x$value1)) {
    x$value1 <- get_decimal_year_value(survey_year[2], x$value1)
  }
  return(x)
}

#' check_inputs_predict_request_year_mean
#' @inheritParams predict_request_year_mean
#' @return logical
#' @noRd
check_inputs_predict_request_year_mean <- function(survey_year, survey_mean, proxy) {

  # CHECK for incorrect NA's
  if (anyNA(survey_year)) {
    rlang::abort(c('`survey_year` can\'t contain missing values:',
                   x = sprintf('Found %s missing values in `survey_year.`',
                               sum(is.na(survey_year)))
                   ))
  }

  # CHECK for correct classes
  if (!is.numeric(survey_year))
    rlang::abort(c('`survey_year` must be a numeric or integer vector:',
                   x = sprintf('You\'ve supplied an object of class %s.',
                               class(survey_year))))
  if (!is.numeric(survey_mean))
    rlang::abort(c('`survey_mean` must be a numeric or integer vector:',
                   x = sprintf('You\'ve supplied an object of class %s.',
                               class(survey_mean))))
  if (!is.numeric(proxy$value0))
    rlang::abort(c('`proxy$value0` must be a numeric or integer vector:',
                   x = sprintf('You\'ve supplied an object of class %s.',
                               class(proxy$value0))))
  if (!is.null(proxy$value1) & !is.numeric(proxy$value1))
    rlang::abort(c('`proxy$value1` must be a numeric or integer vector:',
                   x = sprintf('You\'ve supplied an object of class %s.',
                               class(proxy$value1))))
  if (!is.numeric(proxy$req_value))
    rlang::abort(c('`proxy$req_value` must be a numeric or integer vector:',
                   x = sprintf('You\'ve supplied an object of class %s.',
                               class(proxy$req_value))))

  # CHECK for compatible lengths
  if (length(survey_year) > 2)
    rlang::abort(c('`survey_year` has too many values.',
                   i = 'You can\'t calculate a predicted mean for more than two surveys.'))
  if (length(survey_mean) > 2)
    rlang::abort(c('`survey_mean` has too many values.',
                   i = 'You can\'t calculate a predicted mean for more than two surveys.'))
  if (length(survey_year) != length(survey_mean))
    rlang::abort(c('`survey_year` and `survey_mean` must have compatible lengths:',
                   x = sprintf('`survey_year` has length %s.',
                               length(survey_year)),
                   x = sprintf('`survey_mean` has length %s.',
                               length(survey_mean))))

  if (length(survey_mean) == 2 & is.null(proxy$value1))
    rlang::abort(c('You supplied two survey means, but only a proxy value for the first year.',
                   i = 'Pass an additonal value to argument `value1 in `proxy`.'))

   if (get_weights(survey_year[1])[1] != 1) {
    if (length(proxy$value0) != 2)
      rlang::abort(
        c('`proxy$value0` has the wrong length.',
          i = 'You must supply two calendar year values, since you supplied a decimal survey year.'))
  }
  if (length(survey_year) == 2) {
    if (get_weights(survey_year[2])[1] != 1) {
      if (length(proxy$value1) != 2)
        rlang::abort(
          c('`proxy$value1` has the wrong length.',
            i = 'You must supply two calendar year values, since you supplied a decimal survey year.'))
    }
  }
}
