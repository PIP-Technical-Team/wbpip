#' Compute default PIP statistics from microdata
#'
#' Compute poverty and distributional statistics
#'
#' @param welfare numeric: A vector of income or consumption values
#' @param povline numeric: Monthly poverty line in international dollars
#' @param population numeric: A vector of population weights, optional, a vector
#' of 1s if not specified.
#' @param requested_mean numeric: Welfare mean in international dollars
#' @param popshare numeric: Share of population for which the corresponding
#' quantile is desired. Default .5 (i.e., weighted median)
#' @param default_ppp numeric: Default purchasing power parity
#' @param ppp numeric: PPP requested by user
#' @param distribution_type character: The type of data to be provided as imput
#' to the function.
#' @return list
#' @export
#'
compute_pip_stats <- function(welfare,
                              povline,
                              population = NULL,
                              requested_mean = NULL,
                              popshare = NULL,
                              default_ppp = 1,
                              ppp = NULL,
                              distribution_type = c("microdata",
                                                    "group",
                                                    "aggregate",
                                                    "imputed")
) {

  type <- match.arg(distribution_type)

  if (type == "microdata") {

    out <- md_compute_pip_stats(welfare        = welfare,
                                povline        = povline,
                                population     = population,
                                requested_mean = requested_mean,
                                popshare       = popshare,
                                default_ppp    = default_ppp,
                                ppp            = ppp)

    return(out)

  } else if (type == "group") {

    out <- gd_compute_pip_stats(welfare        = welfare,
                                povline        = povline,
                                population     = population,
                                requested_mean = requested_mean,
                                popshare       = popshare,
                                default_ppp    = default_ppp,
                                ppp            = ppp)

    return(out)

  } else if (type == "aggregate") {

    return(NA)

  } else if (type == "imputed") {

    return(NA)

  } else {

    return(NA)
  }
}
