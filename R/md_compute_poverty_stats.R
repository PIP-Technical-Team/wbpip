md_compute_poverty_stats <- function(welfare, povline, weight = NULL) {

  ## set all weights to 1 if none are supplied
  if (is.null(weight) == TRUE) {
    weight <- rep(1, length(welfare))
  }

  headcount <- 0
  gap <- 0
  severity <- 0
  watt8 <- 0

  for (i in seq_along(welfare)) {

    weight_i <- weight[i]
    welfare_i <- welfare[i]

    if (welfare_i <= povline) {

      #in this block I set na.rm = TRUE for sums to ensure that in the presence of na.rm function
      #wont return missing values
      headcount <- sum(headcount, weight_i, na.rm = TRUE)
      gap_i <- 1 - welfare_i / povline
      gap <- sum(gap, weight_i * gap_i, na.rm = TRUE)
      severity <- sum(severity, weight_i * gap_i ^ 2)
      if (welfare_i > 0) { # Is this check needed no negative welfare value should make it to the application
        watt8 <- sum(watt8, weight_i * log(povline / welfare_i), na.rm = TRUE)
      }

    }
  }

  return(list(
    headcount = headcount,
    poverty_gap = gap,
    poverty_severity = severity,
    watts = watt8
  ))
}


clean_inputs_md_compute_poverty_stats <- function(welfare, weight){

  # Remove missing values
  if (anyNA(welfare) | anyNA(weight)) {
    if (anyNA(welfare)) {
      if (sum(is.na(welfare)) == 1) {
        msg <- sprintf('Info: Found 1 missing value in `welfare`. This observation was removed.')
      } else {
        msg <- sprintf('Info: Found %s missing values in `welfare`. These observations were removed.',
                       sum(is.na(welfare)))
      }
      rlang::inform(msg)
    }
    if (anyNA(weight)) {
      if (sum(is.na(weight)) == 1) {
        msg <- sprintf('Info: Found 1 missing value in `weight`. This observation was removed.')
      } else {
        msg <- sprintf('Info: Found %s missing values in `weight`. These observations were removed.',
                       sum(is.na(weight)))
      }
      rlang::inform(msg)
    }
    weight_no_na <- weight[!is.na(welfare)]
    weight_no_na <- weight_no_na[!is.na(weight_no_na)]
    welfare_no_na <- welfare[!is.na(weight)]
    welfare_no_na <- welfare_no_na[!is.na(welfare_no_na)]
    weight <- weight_no_na
    welfare <- welfare_no_na
  }

  # Remove negative values
  if (any(welfare < 0) | any(weight < 0)) {
    if (any(welfare < 0)) {
      if (sum(welfare < 0) == 1) {
        msg <- sprintf('Info: Found 1 negative value in `welfare`. This observation was removed.')
      } else {
        msg <- sprintf('Info: Found %s negative values in `welfare`. These observations were removed.',
                       sum(welfare < 0))
      }
      rlang::inform(msg)
    }
    if (any(weight < 0)) {
      if (sum(weight < 0) == 1) {
        msg <- sprintf('Info: Found 1 negative value in `weight`. This observation was removed.')
      } else {
        msg <- sprintf('Info: Found %s negative values in `weight`. These observations were removed.',
                       sum(weight < 0))
      }
      rlang::inform(msg)
    }
    weight_no_negative <- weight[!welfare < 0]
    weight_no_negative <- weight_no_negative[!weight_no_negative < 0]
    welfare_no_negative <- welfare[!weight < 0]
    welfare_no_negative <- welfare_no_negative[!welfare_no_negative < 0]
    weight <- weight_no_negative
    welfare <- welfare_no_negative
  }

  # Order welfare and weight if welfare is unsorted
  if (is.unsorted(welfare)) {
    welfare_ordered <- order(welfare)
    weight <- weight[welfare_ordered]
    welfare <- sort(welfare)
  }

  out <- list(welfare = welfare, weight = weight)
  return(out)

}

check_inputs_md_compute_poverty_stats <- function(welfare, weight){
  if (!is.numeric(weight))
    rlang::abort(c('`weight` must be a numeric or integer vector:',
                   x = sprintf('You\'ve supplied an object of class %s.', class(weight))))
  if (!is.numeric(welfare))
    rlang::abort(c('`welfare` must be a numeric or integer vector:',
                   x = sprintf('You\'ve supplied an object of class %s.', class(welfare))))
  if (length(weight) != length(welfare))
    rlang::abort(c('`welfare` and `weight` must have compatible lengths:',
                   x = sprintf('`welfare` has length %s.', length(welfare)),
                   x = sprintf('`weight` has length %s.', length(weight))))
}



