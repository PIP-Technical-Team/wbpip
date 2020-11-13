#' Clean Group Data
#'
#' @param dt Data frame.
#' @param welfare numeric: welfare vector whose form depends on on `type` (SE).
#' @param population numeric: population vector whose form depends on on `type` (SE).
#' @param data_type numeric: Type of data.
#' If `type = 1`, `population` must be the
#' cumulative proportion of population and `welfare` must be the cumulative
#' proportion of income held by that proportion of the population (Lorenz Curve).
#' If `type = 2`, `population` must be the proportion of population and
#' `welfare` must be the proportion of income.
#' If `type = 5`, then `population` must be the Percentage
#' of the population in a given interval of incomes, whereas `welfare` must be
#' the mean income of that interval. Default is 1.
#'
#' @return dataframe in data.table format
#' @export
#' @import data.table
#'
gd_clean_data <- function(dt,
                          welfare,
                          population,
                          data_type = 1) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #---------   Extract vectors   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # to make sure we can use non-standard evaluation... (do we want NSE?)
  # welfare    <- deparse(substitute(welfare))
  # population <- deparse(substitute(population))

  if (!(inherits(dt, "data.table"))) {
    setDT(dt)
  }

  welfare    <- dt[, get(welfare)]
  population <- dt[, get(population)]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #---------   Check inputs   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  check_gd_input(population = population,
                 welfare    = welfare,
                 data_type  = data_type)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #---------   Build back type 1   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (data_type == 1) {
    return(data.table::data.table(
      population = population,
      welfare    = welfare
    ))
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #---------   Standardize types 2 and 5   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Standardize according to input distribution
  if (data_type %in% c(3, 5)) {
    ngd <- standardize_type5(population = population,
                              welfare = welfare)

    # Check data was standardized correctly
    check_gd_input(population = ngd$population,
                   welfare    = ngd$welfare,
                   data_type  = 1)
  }

  if (data_type == 2) {

    #---------  make sure data is sorted ---------
    ngd <- standardize_type2(population = population,
                              welfare = welfare)

    check_gd_input(population = ngd$population,
                   welfare    = ngd$welfare,
                   data_type  = 1)
  }

  return(ngd)
}

#' Standardize type 5
#'
#' @param welfare numeric: welfare vector
#' @param population numeric: population vector
#'
#' @return data.table
#' @export
#'
standardize_type5 <- function(welfare,
                              population) {

  if (sum(population) == 100) {
    population <- population/100
    cli::cli_alert_info("variable {.val population} has been rescaled to
                        range (0,1]", wrap = TRUE)
  }

  # sum_population      <- sum(population)
  mean_welfare        <- sum(population * welfare)
  min_welfare         <- min(welfare)
  max_welfare         <- max(welfare)

  # Compute points on the lorenz curve
  lorenz_pop     <- cumsum(population)
  share_welfare  <- population * (welfare/mean_welfare)
  lorenz_welfare <- cumsum(share_welfare)

  return(data.table::data.table(
    population = lorenz_pop,
    welfare    = lorenz_welfare
  )
  )
}

#' standardize_type2
#' Standardize grouped data of type 2 distribution
#' @param population numeric: population, proportion of population
#' @param welfare numeric: welfare, proportion of income / consumption
#' @param min_welfare_default numeric:
#' @param max_welfare_default numeric:
#'
#' @return list
#' @export
#'
standardize_type2 <- function(population,
                              welfare) {

  #--------- Make sure variables go from 0 to 1 ---------

  varrescale <- vector()
  if (sum(population) == 100) {

    population <- population/100
    varrescale[length(varrescale) + 1] <- "population"
  }
  if (sum(welfare) == 100) {

    welfare <- welfare/100
    varrescale[length(varrescale) + 1] <- "welfare"
  }

  cli::cli_alert_info("variable{?s} {.val {varrescale}} ha{?s/ve} been rescaled to
                      range (0,1]", wrap = TRUE)

  #--------- Cumulative vectors ---------
  lorenz_pop     <- cumsum(population)
  lorenz_welfare <- cumsum(welfare)

  return(data.table::data.table(
    population = lorenz_pop,
    welfare    = lorenz_welfare
    )
    )
}

#' Check Group Data inputs
#'
#' @inheritParams gd_clean_data
#'
#' @return
#' @export
#'
check_gd_input <- function(population,
                           welfare,
                           data_type) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #---------   Basic checks   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Check welfare and population
  assertthat::assert_that(length(population) == length(welfare))
  assertthat::is.number(population)
  assertthat::is.number(welfare)

  assertthat::assert_that(sum(is.na(population)) == 0,
                          msg = "Data can't have NA in population")
  assertthat::assert_that(sum(is.na(welfare)) == 0,
                          msg = "Data can't have NA in welfare")

  # Check data type
  assertthat::assert_that(length(data_type) == 1)
  assertthat::assert_that(data_type %in% c(1, 2, 5))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #---------   Checks based on type   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #--------- Type 1 ---------
  if (data_type == 1) {

    #--------- check vector cumsum up to one ---------
    share_pop <- c(population[1], diff(population))
    share_wel <- c(welfare[1], diff(welfare))

    assertthat::assert_that(sum(share_pop) == 1,
                            msg = "Share of `population` does not sum up to 1")

    assertthat::assert_that(sum(share_wel) == 1,
                            msg = "Share of `welfare` does not sum up to 1")

    #--------- make sure  share of income is always increasing ---------
    # normalize welfare by population
    norm_wel <- diff(share_wel/share_pop)

    assertthat::assert_that(all(norm_wel >= 0),
                            msg = paste0("share of `welfare` must increase with each\n",
                                         "subsequent bin relative to its corresponging\n",
                                         "population. Make sure data is sorted correctly."))

  }

  #--------- type 2 ---------

  if (data_type == 2) {

    assertthat::assert_that(sum(welfare) %in% c(1, 100))
    assertthat::assert_that(sum(population) %in% c(1, 100))

  }


  #--------- type 5 (or 3) ---------

  # Still need more tests
  if (data_type %in% c(3, 5)) {

    assertthat::assert_that(sum(population) %in% c(1, 100))

  }

}



#' standardize_type5_old
#' Standardize grouped data of type 5 distribution
#' @param population numeric: population, in percentage of the population in a given interval of incomes.
#' @param welfare numeric: welfare, the mean income / consumption of that interval
#' @param min_welfare_default numeric:
#' @param max_welfare_default numeric:
#'
#' @return list
#' @export
#'
standardize_type5_old <- function(population,
                                  welfare,
                                  min_welfare_default,
                                  max_welfare_default) {
  nobs                <- length(population)
  lorenz_pop          <- vector(mode = "numeric", length = nobs)
  lorenz_welfare      <- vector(mode = "numeric", length = nobs)

  sum_population      <- sum(population)
  sum_welfare         <- sum(population * welfare)
  min_welfare         <- min(welfare)
  max_welfare         <- max(welfare)

  min_welfare_default <- ifelse(min_welfare < min_welfare_default,
                                min_welfare,
                                min_welfare_default)

  max_welfare_default <- ifelse(max_welfare > max_welfare_default,
                                max_welfare,
                                max_welfare_default)

  mean_welfare        <- sum_welfare / sum_population

  # Compute points on the lorenz curve
  lorenz_pop[1]     <- population[1] / sum_population
  lorenz_welfare[1] <- population[1] * welfare[1] / sum_population / mean_welfare
  for (i in seq(2, nobs)) {
    lorenz_pop[i] <- population[i] / sum_population + lorenz_pop[i-1]
    lorenz_welfare[i] <- population[i] * welfare[i] / sum_population / mean_welfare + lorenz_welfare[i-1]
  }

  return(data.table(population = lorenz_pop,
                    welfare    = lorenz_welfare))
}

#' standardize_type2_old
#' Standardize grouped data of type 2 distribution
#' @param population numeric: population, proportion of population
#' @param welfare numeric: welfare, proportion of income / consumption
#' @param min_welfare_default numeric:
#' @param max_welfare_default numeric:
#'
#' @return list
#' @export
#'
standardize_type2_old <- function(population,
                                  welfare,
                                  min_welfare_default,
                                  max_welfare_default) {
  nobs                <- length(population)
  lorenz_pop          <- vector(mode = "numeric", length = nobs)
  lorenz_welfare      <- vector(mode = "numeric", length = nobs)
  sum_population      <- sum(population)
  sum_welfare         <- sum(welfare)
  min_welfare         <- min(welfare)
  max_welfare         <- max(welfare)
  min_welfare_default <- if (min_welfare < min_welfare_default) min_welfare else min_welfare_default
  max_welfare_default <- if (max_welfare < max_welfare_default) max_welfare else max_welfare_default
  lorenz_pop[1]       <- population[1] / sum_population
  lorenz_welfare[1]   <- welfare[1] / sum_welfare
  for (i in seq(2, nobs)) {
    lorenz_pop[i] <- lorenz_pop[i - 1] + population[i] / sum_population
    lorenz_welfare[i] <- lorenz_welfare[i - 1] + welfare[i] / sum_welfare
  }

  return(list(lorenz_pop = lorenz_pop,
              lorenz_welfare = lorenz_welfare))
}
