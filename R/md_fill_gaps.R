#' Fill gaps for microdata
#'
#' @param request_year integer: A value with the request year.
#' @param data list: A list with one or two data frames containing survey data.
#' @param predicted_request_mean numeric: A vector with one or two survey means.
#' @param survey_year numeric: A vector with one or two survey years.
#' @param poverty_line numeric: Daily poverty line in international dollars.
#'
#' @noRd
md_fill_gaps <- function(request_year, survey_year,
                         data, predicted_request_mean,
                         poverty_line) {

  # Clean data
  if ('weight' %in% names(data$df0)) {
    data$df0 <-
      md_clean_data(data$df0,
                    welfare = 'welfare',
                    weight = 'weight')$data
  } else {
    data$df0 <-
      md_clean_data(data$df0, welfare = 'welfare')$data
  }

  if (!is.null(data$df1)) {
    if ('weight' %in% names(data$df1)) {
      data$df1 <-
        md_clean_data(data$df1,
                      welfare = 'welfare',
                      weight = 'weight')$data
    } else {
      data$df1 <-
        md_clean_data(data$df1, welfare = 'welfare')$data
    }
  }

  # Calculate poverty stats
  if (length(predicted_request_mean) == 1) {

    # Calculate poverty statistics for the request year
    out <- md_compute_pip_stats(welfare = data$df0$welfare,
                                weight = data$df0$weight,
                                povline = poverty_line,
                                default_ppp = 1,
                                requested_mean = predicted_request_mean)

  } else {

    # Calculate statistics for the first survey year
    dl0 <- md_compute_pip_stats(welfare = data$df0$welfare,
                                weight = data$df0$weight,
                                povline = poverty_line,
                                default_ppp = 1,
                                requested_mean = predicted_request_mean[1])

    # Calculate statistics for the second survey year
    dl1 <- md_compute_pip_stats(welfare = data$df1$welfare,
                                weight = data$df1$weight,
                                povline = poverty_line,
                                default_ppp = 1,
                                requested_mean = predicted_request_mean[2])

    # Calculate poverty statistics for the request year (weighted average)
    out <- fg_adjust_poverty_stats(dl0, dl1, survey_year, request_year)
  }

  return(out)
}
