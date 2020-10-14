md_clean_inputs_compute_poverty_stats <- function(welfare, weight){

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
