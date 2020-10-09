md_compute_poverty_stats <- function(welfare, povline, weights) {


  # headcount = pg = p2 = watt8 = lastY = sumY <- 0
  headcount <- 0
  pg <- 0
  p2 <- 0
  watt8 <- 0
  lastY <- 0
  sumY <- 0

  for (i in seq_along(welfare[["welfare"]])) {

    wi <- welfare[['weight']][i]
    yi <- welfare[['welfare']][i]

    if (yi <= povline) {

      headcount <- sum(headcount, wi)
      pgi <- 1 - yi / povline
      pg <- sum(pg, wi * pgi)
      p2 <- sum(p2, wi * pgi^2)
      if (yi > 0) { # Is this check needed no negative welfare value should make it to the application
        watt8 <- sum(watt8, wi * log(povline/yi))
      }
      sumY <- sum(sumY, yi * wi) # Not used...?
      lastY <- yi

    } else {
      break
    }
  }

  return(list(
    headcount = headcount,
    poverty_gap = pg,
    poverty_severity = p2,
    watts = watt8
  ))
}

