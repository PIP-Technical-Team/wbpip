#' @import data.table
NULL

# Add global variables to avoid NSE notes in R CMD check
if (getRversion() >= '2.15.1')
  utils::globalVariables(
    c('weight')
  )

#' Clean data (micro)
#'
#' Clean microdata to be used in PIP methods.
#'
#' `md_clean_data()` returns a list of elements whose main object is a
#' data.table with the necessary transformations to be included in PIP methods.
#' Data is available in element `$data`. The other elements provide the number
#' of observations that were modified depending on test performed. The name of
#' elements are in the form p_s, where p (or prefix) refers to the test and s
#' (the suffix) refers to the name of the variable evaluated.
#'
#' Prefixes are:
#' * nna: Number of NA in variable
#' * nng: Number of negative values
#' * ina: Index of obs with NA in variable
#' * ing: Index of obs with negative values
#'
#' @param dt data.frame: A table with survey data.
#' @param welfare character: Name of welfare column.
#' @param weight character: Name of weight column. Optional.
#' @param quiet logical: If TRUE output messages are suppressed.
#' @return list
#' @keywords internal
md_clean_data <- function(dt, welfare, weight = NULL, quiet = FALSE) {

  # Convert to data.table
  if (!(inherits(dt, "data.table"))) {
    data.table::setDT(dt)
  }

  # Column names
  welfare_col <- welfare
  weight_col <- weight

  # Create output list
  ll <- vector(mode = 'list')

  #--------- WELFARE ---------

  # Check for missing welfare values
  nna <- dt[is.na(get(welfare_col)) , .N]
  if (nna > 0) {
    # Get which rows w/ missing values
    ina <- dt[, which(is.na(get(welfare_col)))]
    # Remove rows
    dt  <- dt[!is.na(get(welfare_col))]
    # Add to output list
    ll[[paste0('nna_', welfare_col)]] <- nna
    ll[[paste0('ina_', welfare_col)]] <- ina
    # Print message
    if (!quiet) {
      nna_msg(nna, welfare_col)
    }
  }

  # Check for negative welfare values
  nng <- dt[get(welfare_col) < 0 , .N]
  if (nng > 0) {
    # Get which rows w/ negative values
    ing <- dt[, which(get(welfare_col) < 0)]
    # Remove rows
    dt <- dt[get(welfare_col) >= 0]
    # Add to output list
    ll[[paste0('nng_', welfare_col)]] <- nng
    ll[[paste0('ing_', welfare_col)]] <- ing
    # Print message
    if (!quiet) {
      nng_msg(nng, welfare_col)
    }
  }

  # Order by increasing welfare values
  if (is.unsorted(dt[[welfare_col]])) {
    data.table::setorderv(dt, welfare_col)
    if (!quiet) {
      cli::cli_alert_info('Data has been sorted by variable {.val {welfare_col}}')
    }
  }

  #--------- WEIGHT ---------

  if (!is.null(weight_col)) {

    # Check for missing weight values
    nna <- dt[is.na(get(weight_col)) , .N]
    if (nna > 0) {
      # Get which rows w/ missing values
      ina <- dt[, which(is.na(get(weight_col)))]
      # Remove rows
      dt <- dt[!is.na(get(weight_col))]
      # Add to output list
      ll[[paste0('nna_', weight_col)]] <- nna
      ll[[paste0('ina_', weight_col)]] <- ina
      # Print message
      if (!quiet) {
        nna_msg(nna, weight_col)
      }
    }

    # Check for negative weight values
    nng <- dt[get(weight_col) <= 0 , .N]
    if (nng > 0) {
      # Get which rows w/ negative values
      ing <- dt[, which(get(weight_col) <= 0)]
      # Remove rows
      dt <- dt[get(weight_col) > 0]
      # Add to output list
      ll[[paste0('nng_', weight_col)]] <- nng
      ll[[paste0('ing_', weight_col)]] <- ing
      # Print message
      if (!quiet) {
        nng_msg(nng, weight_col)
      }
    }
  } else {

    # Add weight column
    dt[, weight := 1]
    if (!quiet) {
      cli::cli_alert_info(
        'since {.val weight} is not provided, variable {.field `weight = 1`} has been created',
        wrap = TRUE)
    }
  }
  # Add data to output list
  ll[['data']] <- dt

  return(ll)

}

#' nng_msg
#' @noRd
nng_msg <- function(nng, x) {
  cli::cli_alert_info("{nng} negative values in variable
                      {.val {x}} were dropped", wrap = TRUE)
  invisible()
}

#' nna_msg
#' @noRd
nna_msg <- function(nna, x) {
  cli::cli_alert_info("{nna} NA values in variable
                      {.val {x}} were dropped", wrap = TRUE)
  invisible()
}
