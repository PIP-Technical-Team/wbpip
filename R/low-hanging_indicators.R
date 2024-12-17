#' Calculate the number of poor using headcount and population
#'
#' @param headcount A numeric vector giving the headcount ratio
#' @param pop A numeric vector giving population size
#'
#' @return A numeric vector giving number of poor for each observation
#' @export
get_number_poor <- function(headcount, pop){

  # headcount must be a ratio
  hc_not_ratio <- which_not_ratio(list(headcount)) # headcounts not between 0 and 1
  headcount[hc_not_ratio] <- NA

  l <- as.list(environment())

  # Number of poor
  np <- headcount*pop

  # treat special values
  # obs_na       <- which_na(l) # Find NA values
  obs_negative <- which_negative(l) # Find negatives


  np[obs_negative] <- NA

  # Return
  return(np)

}



#' Calculate average shortfall for the poor using headcount, poverty gap ratio, and poverty line
#'
#' @param headcount Numeric vector giving headcount ratio, i.e. P0 FGT measure
#' @param povgap Numeric vector giving poverty gap ratio, i.e. P1 FGT measure
#' @param povline Numeric vector giving the poverty line
#'
#' @return Numeric vector giving average shortfall for the poor in the units of the poverty line
#' @export
get_average_shortfall <- function(headcount, povgap, povline){

  # Input checks
  l <- as.list(environment())

  # Average Shortfall
  av_sf <- povline*povgap/headcount # z times P1/P0

  # treat special values
  obs_negative         <- which_negative(l) # Find negatives
  obs_not_ratio        <- which_not_ratio(list(headcount, povgap))
  av_sf[obs_negative]  <- NA
  av_sf[obs_not_ratio] <- NA

  # Return
  return(av_sf)

}




#' Calculate total shortfall from headcount, poverty gap, and poverty line
#'
#' @param headcount Numeric vector giving headcount ratio, i.e. P0 FGT measure
#' @param pop Numeric vector giving population size
#' @param povgap Numeric vector giving poverty gap ratio, i.e. P1 FGT measure
#' @param povline Numeric vector giving the poverty line
#'
#' @return Numeric vector giving total shortfall in the same units as the poverty line
#' @export
get_total_shortfall <- function(headcount, pop, povgap, povline){

  # Input checks not necessary: all checked in function dependencies

  # Total Shortfall
  tot_sf <- get_number_poor(
    headcount = headcount, pop = pop
  )*get_average_shortfall(
    headcount, povgap, povline
  )

  # Return
  return(tot_sf)
}



#' Calculate income gap ratio from headcount and poverty gap
#'
#' @param headcount Numeric vector giving headcount ratio, i.e. P0 FGT measure
#' @param povgap Numeric vector giving poverty gap ratio, i.e. P1 FGT measure
#'
#' @return Numeric vector giving the income gap ratio
#' @export
get_income_gap_ratio <- function(headcount, povgap){

  # Input checks
  l <- as.list(environment())

  # income gap
  income_gap <- povgap/headcount

  # Find negatives and non-ratios to make NAs
  obs_negative               <- which_negative(list(headcount, povgap)) # Find negatives
  obs_not_ratio              <- which_not_ratio(list(headcount, povgap))
  income_gap[obs_not_ratio]  <- NA
  income_gap[obs_negative]   <- NA

  # return
  return(income_gap)

}



#' Calculate Palma ratio using decile information
#'
#' @param decile1 Numeric vector giving either welfare or welfare share for first decile
#' @param decile2 Numeric vector giving either welfare or welfare share for second decile
#' @param decile3 Numeric vector giving either welfare or welfare share for third decile
#' @param decile4 Numeric vector giving either welfare or welfare share for fourth decile
#' @param top10 Numeric vector giving either welfare or welfare share for tenth decile
#' @param bottom40 numeric: sum of deciles 1 to 4.
#'
#' @return Numeric vector giving the Palma ratio, the ratio of top 10% to bottom 40% welfare
#' @export
get_palma_ratio <- function(top10,
                               bottom40 = NULL,
                               decile1  = NULL,
                               decile2  = NULL,
                               decile3  = NULL,
                               decile4  = NULL){

  # Input checks
  l <- as.list(environment())
  if (is.null(bottom40)) {

    # create bottom 40
    obs_negative <-
      list(top10, decile1, decile2, decile3, decile4) |>
      which_negative() # Find negatives

    bottom40 <- decile1 + decile2 + decile3 + decile4
  } else {
    obs_negative <- which_negative(list(top10, bottom40)) # Find negatives
  }

  # Palma ratio
  palma <- top10/bottom40

  # treat special values

  palma[obs_negative] <- NA

  # return
  return(palma)

}


#' Calculate ratio of the top 10% to the bottom 10% of the welfare distribution
#'
#' @param top10 Numeric vector giving the welfare of the 10th decile - i.e. the top 10%
#' @param bottom10 Numeric vector giving the welfare of the 1st decile - i.e. the bottom 10%
#'
#' @return Numeric vector giving the ratio of the top 10% to the bottom 10% of the welfare distribution
#' @export
get_9010_ratio <- function(
    top10,
    bottom10){

  # Input Checks
  l <- as.list(environment())

  # Decile ratio
  ratio <- top10/bottom10

  # treat special values
  obs_negative  <- which_negative(l) # Find negatives

  ratio[obs_negative] <- NA

  # return
  return(ratio)


}



#' find unique obs with specific characteristics
#'
#' `which_na` finds out which observations are NA in a list of vectors.
#'
#' `which_negative` finds out which observations are negative in a list of
#' vectors.
#'
#' `which_not_ratio` finds out which observations are not between 0 and 1 in a
#' list of vectors
#'
#' @param l list of vectors
#'
#' @return numeric vector with index
#' @keywords internal
which_na <- function(l) {
  lapply(l, \(x) which(is.na(x))) |>
    unlist() |>
    unique()
}


#' @describeIn which_na finds out which observations are negative in a list of vectors
which_negative <- function(l) {
  lapply(l, \(x) which(x < 0)) |>
    unlist() |>
    unique()
}

#' @describeIn which_na finds out which observations are not between 0 and 1 in a
#' list of vectors
which_not_ratio <- function(l) {
  lapply(l, \(x) which(x < 0 | x > 1)) |>
    unlist() |>
    unique()
}


