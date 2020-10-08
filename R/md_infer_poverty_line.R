infer_poverty_line <- function(welfare,
                               weight,
                               popshare,
                               sum_weights) {

  headcount <- 0
  # pr.h <- popshare # headcount defined as popshare
  wtile <- popshare * sum_weights # Number of people below PL in survey sample
  lastY <- 0
  pl <- NA

  for (i in seq_along(welfare)) {

    wi <- weight[i]
    yi <- welfare[i]

    if (sum(headcount, wi) < wtile) {
      headcount <- sum(headcount, wi)
      lastY <- yi
    } else {
      pl <- lastY + (yi - lastY) * (wtile - headcount) / wi # infer poverty line from h
      break
    }
  }

  return(pl)

}
