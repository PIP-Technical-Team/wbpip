#' Estimate distribution (grouped data)
#'
#' @param gd_type integer: Type of data distribution
#' If `gd_type = 1` `population` must be the cumulative proportion of
#' population and `welfare` must be the cumulative proportion of income held by
#' that proportion of the population (Lorenz curve). If `gd_type = 2`,
#' `population` must be the proportion of population and `welfare` must be the
#' proportion of income. If `gd_type = 5`, then `population` must be the
#' Percentage of the population in a given interval of incomes, whereas
#' `welfare` must be the mean income of that interval.
#' @param mean numeric: Data mean
#' @inheritParams gd_compute_pip_stats
#'
#' @return data.frame
#' @export
#'
#' @examples
#' welfare <- c(24.84, 35.8, 45.36, 55.1, 64.92, 77.08, 91.75, 110.64, 134.9,
#' 167.76, 215.48, 261.66, 384.97)
#' population <- c(0.92, 2.47, 5.11, 7.9, 9.69, 15.24, 13.64, 16.99, 10, 9.78,
#' 3.96, 1.81, 2.49)
#'
#' gd_estimate_distribution(welfare = welfare,
#' population = population,
#' gd_type = 5,
#' mean = 109.9,
#' povline = 89)

gd_estimate_distribution <- function(welfare,
                                     population,
                                     gd_type,
                                     mean,
                                     povline,
                                     popshare = NULL
) {
  # STEP 1: Standardize group data
  # Check inputs
  check_inputs_gd_clean_data(
    population = population,
    welfare = welfare,
    gd_type = gd_type
  )

  # Standardize data according to type
  if (gd_type == 1) {
    res <- gd_standardize_type1(
      population = population,
      welfare = welfare
    )
  } else if (gd_type == 2) {
    res <- gd_standardize_type2(
      population = population,
      welfare = welfare
    )
  } else if (gd_type == 5) {
    res <- gd_standardize_type5(
      population = population,
      welfare = welfare
    )
  }

  # Check that data was standardized correctly
  validate_output_gd_clean_data(
    population = res$population,
    welfare = res$welfare
  )

  # STEP 2: Estimate distribution
  out <- gd_compute_pip_stats(welfare = res$welfare,
                              population = res$population,
                              povline = povline,
                              mean = mean,
                              popshare = popshare)

  # STEP 3: Return results
  return((out))
}

