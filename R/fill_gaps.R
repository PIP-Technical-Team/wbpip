# Add global variables to avoid NSE notes in R CMD check
if (getRversion() >= '2.15.1')
  utils::globalVariables(
    c('df0', 'predicted_request_mean', 'request_year',
      'survey_year', 'data', 'poverty_line')
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
#' @param distribution_type character: Type of distribution, either micro,
#'   group, aggregate or imputed.
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
#'   distribution_type = 'micro',
#'   poverty_line = 1.9)
#'
#' # Interpolatation (monotonic)
#' res <- fill_gaps(
#'   request_year = 2005,
#'   survey_year = c(2000, 2010),
#'   data = list(df0 = md_ABC_2000_income, df1 = md_ABC_2010_income),
#'   predicted_request_mean = c(13, 13),
#'   distribution_type = 'micro',
#'   poverty_line = 1.9)
#'
#' # Interpolatation (non-monotonic)
#' res <- fill_gaps(
#'   request_year = 2005,
#'   survey_year = c(2000, 2010),
#'   data = list(df0 = md_ABC_2000_income, df1 = md_ABC_2010_income),
#'   predicted_request_mean = c(14, 17),
#'   distribution_type = 'micro',
#'   poverty_line = 1.9)
#'
#' @export
fill_gaps <- function(request_year,
                      data = list(df0, df1 = NULL),
                      predicted_request_mean,
                      survey_year,
                      distribution_type =
                        c("micro", "group", "aggregate", "imputed"),
                      poverty_line = 1.9) {

  # CHECKS
  environment(check_inputs_fill_gaps) <- environment()
  check_inputs_fill_gaps()

  # Match arguments
  distribution_type <- match.arg(distribution_type)

  # Calculate poverty stats
  if (distribution_type == 'micro') {
    if (length(predicted_request_mean) == 1) {

      weights0 <- fg_get_weights(data$df0)

      # Calculate poverty statistics for the request year
      out <- md_compute_pip_stats(welfare        = data$df0$welfare,
                                  population     = weights0,
                                  povline        = poverty_line,
                                  default_ppp    = 1,
                                  requested_mean = predicted_request_mean)

    } else {

      weights0 <- fg_get_weights(data$df0)
      weights1 <- fg_get_weights(data$df1)

      # Calculate statistics for the first survey year
      dl0 <- md_compute_pip_stats(welfare        = data$df0$welfare,
                                  population     = weights0,
                                  povline        = poverty_line,
                                  default_ppp    = 1,
                                  requested_mean = predicted_request_mean[1])

      # Calculate statistics for the second survey year
      dl1 <- md_compute_pip_stats(welfare        = data$df1$welfare,
                                  population     = weights1,
                                  povline        = poverty_line,
                                  default_ppp    = 1,
                                  requested_mean = predicted_request_mean[2])

      # Calculate poverty statistics for the request year (weighted average)
      out <- fg_adjust_poverty_stats(dl0, dl1, survey_year, request_year)
    }

    out <- md_fill_gaps(request_year = request_year,
                        data = data,
                        predicted_request_mean = predicted_request_mean,
                        survey_year = survey_year,
                        poverty_line = poverty_line
                        )

  } else if (distribution_type == 'group') {

    message('Note: Grouped functions are not implemented yet.')

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
