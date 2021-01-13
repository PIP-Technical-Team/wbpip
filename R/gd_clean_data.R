#' Clean data (grouped)
#'
#' Clean grouped data to be used in PIP methods.
#'
#' If `gd_type = 1` `population` must be the cumulative proportion of
#' population and `welfare` must be the cumulative proportion of income held by
#' that proportion of the population (Lorenz curve). If `gd_type = 2`,
#' `population` must be the proportion of population and `welfare` must be the
#' proportion of income. If `gd_type = 5`, then `population` must be the
#' Percentage of the population in a given interval of incomes, whereas
#' `welfare` must be the mean income of that interval.
#'
#' @param dt data.frame: A table with survey data.
#' @param welfare character: Name of welfare column.
#' @param population character: Name of population column.
#' @param gd_type numeric: Type of data. See details.
#' @param quiet logical: If TRUE output messages are suppressed.
#'
#' @return data.table
#' @keywords internal
gd_clean_data <- function(dt, welfare, population,
                          gd_type, quiet = FALSE) {

  # Convert to data.table
  if (!(inherits(dt, 'data.table'))) {
    data.table::setDT(dt)
  }

  # Extract vectors
  welfare_vector <- dt[[welfare]]
  population_vector <- dt[[population]]

  # Check inputs
  check_inputs_gd_clean_data(
    population = population_vector,
    welfare = welfare_vector,
    gd_type = gd_type)

  # Standardize data according to type
  if (gd_type == 1) {
    res <- gd_standardize_type1(
      population = population_vector,
      welfare = welfare_vector)
  } else if (gd_type == 2) {
    res <- gd_standardize_type2(
      population = population_vector,
      welfare = welfare_vector)
  } else if (gd_type == 5) {
    res <- gd_standardize_type5(
      population = population_vector,
      welfare = welfare_vector)
  }

  # Check that data was standardized correctly
  validate_output_gd_clean_data(
    population = res$population,
    welfare = res$welfare)

  if (!quiet) {
    cli::cli_alert_info(
      'columns {.val welfare} and {.val {population}} have been rescaled to range (0,1]',
      wrap = TRUE)
  }

  # Overwrite values in supplied data frame
  dt[[welfare]] <- res$welfare
  dt[[population]] <- res$population

  return(dt)
}

#' gd_standardize_type1
#' Standardize grouped data of type 1 distribution.
#' @inheritParams gd_clean_data
#' @return list
#' @noRd
gd_standardize_type1 <- function(population,
                                 welfare) {

  nobs <- length(population)
  sum_population <- population[nobs]
  sum_welfare <- welfare[nobs]

  # Calculate points on the Lorenz curve
  lorenz_pop <- population / sum_population
  lorenz_welfare <- welfare / sum_welfare

  out <- list(
    welfare = lorenz_welfare,
    population = lorenz_pop)

  return(out)
}


#' gd_standardize_type2
#' Standardize grouped data of type 2 distribution.
#' @inheritParams gd_clean_data
#' @return list
#' @noRd
gd_standardize_type2 <- function(population,
                                 welfare) {

  # Calculate points on the Lorenz curve
  lorenz_pop <- cumsum(population) / sum(population)
  lorenz_welfare <- cumsum(welfare) / sum(welfare)

  out <- list(
    welfare = lorenz_welfare,
    population = lorenz_pop)

  return(out)
}


#' gd_standardize_type5
#' Standardize grouped data of type 5 distribution.
#' @inheritParams gd_clean_data
#' @return list
#' @noRd
gd_standardize_type5 <- function(welfare, population) {

  sum_population <- sum(population)
  sum_welfare <- sum(population * welfare)

  # Calculate points on the Lorenz curve
  lorenz_pop <- cumsum(population) / sum_population
  share_welfare <- population * (welfare / sum_welfare)
  lorenz_welfare <- cumsum(share_welfare)

  out <- list(welfare = lorenz_welfare,
              population = lorenz_pop)

  return(out)
}

#' check_inputs_gd_clean_data
#' @inheritParams gd_clean_data
#' @return logical
#' @noRd
check_inputs_gd_clean_data <- function(population,
                                       welfare,
                                       gd_type) {

  # Check welfare and population
  assertthat::assert_that(length(population) == length(welfare))
  assertthat::is.number(population)
  assertthat::is.number(welfare)
  assertthat::assert_that(sum(is.na(population)) == 0,
                          msg = 'Data can\'t have NA in population.')
  assertthat::assert_that(sum(is.na(welfare)) == 0,
                          msg = 'Data can\'t have NA in welfare.')

  # Check data type
  assertthat::assert_that(length(gd_type) == 1)
  assertthat::assert_that(gd_type %in% c(1, 2, 5),
                          msg = 'Data must be of type 1, 2 or 5.')

}

#' validate_output_gd_clean_data
#' @inheritParams gd_clean_data
#' @return logical
#' @noRd
validate_output_gd_clean_data <- function(population,
                                          welfare) {

  # Check that cumulative share sums to 1
  share_pop <- c(population[1], diff(population))
  share_wel <- c(welfare[1], diff(welfare))
  assertthat::assert_that(round(sum(share_pop), digits = 8) == 1,
                          msg = 'Share of `population` does not sum up to 1')
  assertthat::assert_that(round(sum(share_wel), digits = 8) == 1,
                          msg = 'Share of `welfare` does not sum up to 1')

  # Check that share of income is always increasing
  norm_wel <- diff(share_wel / share_pop) # normalize welfare by population
  assertthat::assert_that(all(norm_wel >= 0),
                          msg = paste0('share of `welfare` must increase with each\n',
                                       'subsequent bin relative to its corresponging\n',
                                       'population. Make sure data is sorted correctly.'))

}
