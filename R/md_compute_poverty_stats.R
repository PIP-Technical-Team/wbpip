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






