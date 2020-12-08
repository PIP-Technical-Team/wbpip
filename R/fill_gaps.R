# Add global variables to avoid NSE notes in R CMD check
if (getRversion() >= '2.15.1')
  utils::globalVariables(
    c('df0', 'predicted_request_mean', 'request_year',
      'survey_year', 'data', 'poverty_line',
      'distribution_type', 'default_ppp')
  )

#' Fill gaps
#'
#' Calculate poverty statistics for a request year for which survey data is not
#' available.
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
#' @param predicted_request_mean numeric: A vector with one or two survey means.
#'   See details.
#' @param survey_year numeric: A vector with one or two survey years.
#' @param poverty_line numeric: Daily poverty line in international dollars.
#' @param default_ppp numeric: Default purchasing power parity.
#' @param ppp numeric: PPP request by user.
#' @param distribution_type character: A vector with the type of distribution,
#'   must be either micro, group, aggregate or imputed.
#'
#' @seealso [deflate_welfare_mean()] [predict_request_year_mean()]
#'
#' @examples
#' # Load example data
#' data('md_ABC_2000_income')
#' data('md_ABC_2010_income')
#' md_ABC_2010_income <-
#'    wbpip:::md_clean_data(md_ABC_2010_income,
#'      welfare = 'welfare',
#'      weight = 'weight')$data
#'
#' # Extrapolation
#' res <- fill_gaps(
#'   request_year = 2005,
#'   survey_year = 2000,
#'   data = list(df0 = md_ABC_2000_income),
#'   predicted_request_mean = 13,
#'   default_ppp = 1,
#'   distribution_type = 'micro',
#'   poverty_line = 1.9)
#'
#' # Interpolation (monotonic)
#' res <- fill_gaps(
#'   request_year = 2005,
#'   survey_year = c(2000, 2010),
#'   data = list(df0 = md_ABC_2000_income, df1 = md_ABC_2010_income),
#'   predicted_request_mean = c(13, 13),
#'   default_ppp = 1,
#'   distribution_type = 'micro',
#'   poverty_line = 1.9)
#'
#' # Interpolation (non-monotonic)
#' res <- fill_gaps(
#'   request_year = 2005,
#'   survey_year = c(2000, 2010),
#'   data = list(df0 = md_ABC_2000_income, df1 = md_ABC_2010_income),
#'   predicted_request_mean = c(14, 17),
#'   default_ppp = 1,
#'   distribution_type = 'micro',
#'   poverty_line = 1.9)
#'
#' @export
fill_gaps <- function(request_year,
                      data = list(df0, df1 = NULL),
                      predicted_request_mean,
                      survey_year,
                      default_ppp,
                      ppp = NULL,
                      distribution_type,
                      poverty_line = 1.9) {

  # CHECKS
  environment(check_inputs_fill_gaps) <- environment()
  check_inputs_fill_gaps()

  # Calculate poverty stats
  out <- fg_compute_pip_stats(
    request_year = request_year,
    data = data,
    predicted_request_mean = predicted_request_mean,
    survey_year = survey_year,
    default_ppp = default_ppp,
    ppp = ppp,
    distribution_type = distribution_type,
    poverty_line = poverty_line)

  return(out)

}

#' fg_compute_pip_stats
#'
#' A helper function for calculating interpolated or extrapolated poverty
#' statistics.
#'
#' @inheritParams fill_gaps
#' @return list
#' @noRd
fg_compute_pip_stats <- function(request_year,
                                 data,
                                 predicted_request_mean,
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
  params <- fg_create_params(
    predicted_request_mean = predicted_request_mean,
    data = data,
    poverty_line = poverty_line,
    default_ppp = default_ppp,
    ppp = ppp
  )

  # Calculate poverty stats
  dl <- vector(mode = "list", length = n_surveys)
  for (i in seq_along(type)) {
    dl[[i]] <- do.call(fg_select_compute_pip_stats[[type[i]]], params[[i]])
  }

  # If interpolating between two surveys then calculate
  # a weighted average for the request year
  if (n_surveys == 2) {
    out <- fg_adjust_poverty_stats(dl[[1]], dl[[2]], survey_year, request_year)
    # Else returned the extrapolation for the request year as is
  } else {
    out <- dl[[1]]
  }

  return(out)
}

#' fg_compute_pip_stats
#'
#' Create parameters to be used in `fg_compute_pip_stats()`.
#'
#' @inheritParams fg_compute_pip_stats
#' @return list
#' @noRd
fg_create_params <- function(predicted_request_mean,
                             data,
                             poverty_line,
                             default_ppp,
                             ppp ) {

  # If one survey
  if (length(predicted_request_mean) == 1) {
    params <- list(
      params0 = list(
        welfare = data$df0$welfare,
        population = data$df0$weight,
        povline = poverty_line,
        default_ppp = default_ppp,
        ppp = ppp,
        requested_mean = predicted_request_mean[1]
      )
    )
    # If two surveys (micro or grouped)
  } else {
    params <- list(
      params0 = list(
        welfare = data$df0$welfare,
        population = data$df0$weight,
        povline = poverty_line,
        default_ppp = default_ppp,
        ppp = ppp,
        requested_mean = predicted_request_mean[1]
      ),
      params1 =  list(
        welfare = data$df1$welfare,
        population = data$df1$weight,
        povline = poverty_line,
        default_ppp = default_ppp,
        ppp = ppp,
        requested_mean = predicted_request_mean[2]
      )
    )
  }

  return(params)

}

#' fg_select_compute_pip_stats
#'
#' A small wrapper function for picking the correct `compute_pip_stats()`
#' function depending on distribution type.
#'
#' @noRd
fg_select_compute_pip_stats <- list(
 micro = function(...) md_compute_pip_stats(...),
 group = function(...) gd_compute_pip_stats(...)
)

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

  # Set distributional statistics to missing
  # It does not make sense to interpolate these values
  out[c('polarization', 'gini', 'mld', 'median', 'deciles')] <- NA

  return(out)
}

#' check_inputs_fill_gaps
#' @return logical
#' @noRd
check_inputs_fill_gaps <- function() {

  # Check for incorrect distribution types
  for (i in seq_along(distribution_type)) {
    if (!distribution_type[i] %in% c("micro", "group", "aggregate", "imputed"))
      rlang::abort(
        c('Incorrect value in `distribution_type`:',
          i = "`distribution_type` accepts the following values; 'micro', 'group, 'aggregate' and 'imputed'.",
          x = sprintf("You've supplied '%s'.", distribution_type[i])
          ))
  }

  # Check for names in data input
  if (!'df0' %in% names(data))
    rlang::abort(c('`data$df0` not found.'))
  if (length(data) == 2 & !'df1' %in% names(data))
    rlang::abort(c('`data$df1` not found.'))

  # CHECK for column names
  if (!'welfare' %in% colnames(data$df0))
    rlang::abort(c('`data$df0` needs to contain a column named welfare.'))
  if (!is.null(data$df1) & !'welfare' %in% colnames(data$df1))
    rlang::abort(c('`data$df1` needs to contain a column named welfare.'))

  # CHECK for correct classes
  if (!is.numeric(request_year))
    rlang::abort(c('`request_year` must be a numeric or integer vector:',
                   x = sprintf('You\'ve supplied an object of class %s.',
                               class(request_year))))
  if (!is.numeric(predicted_request_mean))
    rlang::abort(c('`predicted_request_mean` must be a numeric or integer vector:',
                   x = sprintf('You\'ve supplied an object of class %s.',
                               class(predicted_request_mean))))
  if (!is.numeric(survey_year))
    rlang::abort(c('`survey_year` must be a numeric or integer vector:',
                   x = sprintf('You\'ve supplied an object of class %s.',
                               class(survey_year))))
  if (!is.numeric(poverty_line))
    rlang::abort(c('`poverty_line` must be a numeric or integer vector:',
                   x = sprintf('You\'ve supplied an object of class %s.',
                               class(poverty_line))))
  if (!is.numeric(default_ppp))
    rlang::abort(c('`default_ppp` must be a numeric or integer vector:',
                   x = sprintf('You\'ve supplied an object of class %s.',
                               class(default_ppp))))
  if (!is.numeric(data$df0$welfare))
    rlang::abort(c('`data$df0$welfare` must be a numeric or integer vector:',
                   x = sprintf('You\'ve supplied an object of class %s.',
                               class(data$df0$welfare))))
  if (!is.null(data$df1) & !is.numeric(data$df1$welfare))
    rlang::abort(c('`data$df1$welfare` must be a numeric or integer vector:',
                   x = sprintf('You\'ve supplied an object of class %s.',
                               class(data$df1$welfare))))

  # CHECK for compatible lengths
  if (length(survey_year) > 2)
    rlang::abort(c('`survey_year` has too many values.',
                   i = 'You can\'t interpolate between more than two surveys.'))
  if (length(predicted_request_mean) > 2)
    rlang::abort(c('`predicted_request_mean` has too many values.',
                   i = 'You can\'t interpolate between more than two surveys.'))
  if (length(distribution_type) > 2)
    rlang::abort(c('`distribution_type` has too many values.',
                   i = 'You can\'t interpolate between more than two surveys.'))
  if (length(request_year) > 1)
    rlang::abort(c('`request_year` has too many values.',
                   i = 'You can only interpolate or extrapolate to one request year at a time.'))
  if (length(poverty_line) > 1)
    rlang::abort(c('`poverty_line` has too many values.',
                   i = 'You can only supply one poverty line at a time.'))
  if (length(survey_year) != length(predicted_request_mean))
    rlang::abort(c('`survey_year` and `predicted_request_mean` must have compatible lengths:',
                   x = sprintf('`survey_year` has length %s.',
                               length(survey_year)),
                   x = sprintf('`predicted_request_mean` has length %s.',
                               length(predicted_request_mean))))
  if (length(predicted_request_mean) == 2 & is.null(data$df1))
    rlang::abort(c('You supplied two survey means, but only one survey data frame.',
                   i = 'Pass an additonal data frame to argument `df1 in `data`.'))

  # CHECK for incorrect NA's
  if (is.na(request_year)) {
    rlang::abort('`request_year` can\'t be NA.')
  }
  if (anyNA(survey_year)) {
    rlang::abort(c('`survey_year` can\'t contain missing values:',
                   x = sprintf('Found %s missing values in `survey_year.`',
                               sum(is.na(survey_year)))
    ))
  }
  if (anyNA(predicted_request_mean)) {
    rlang::abort(c('`predicted_request_mean` can\'t contain missing values:',
                   x = sprintf('Found %s missing values in `predicted_request_mean`',
                               sum(is.na(predicted_request_mean)))
    ))
  }
  if (is.na(poverty_line)) {
    rlang::abort('`poverty_line` can\'t be NA.')
  }

}
