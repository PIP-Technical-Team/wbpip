#' Clean Group Data
#'
#' @param welfare numeric: welfare vector whose form depends on on `type`.
#' @param population numeric: population vector whose form depends on on `type`.
#' @param type numeric: Type of data.
#' If `type = 1`, `population` must be the
#' cumulative proportion of population and `welfare` must be the cumulative
#' proportion of income held by that proportion of the population (Lorenz Curve).
#' If `type = 2`, `population` must be the proportion of population and
#' `welfare` must be the proportion of income.
#' If `type = 5`, then `population` must be the Percentage
#' of the population in a given interval of incomes, whereas `welfare` must be
#' the mean income of that interval. Default is 1.
#'
#' @return
#' @export
#'
#' @examples
gd_clean_data <- function(welfare,
                          population,
                          type = 1) {

}
