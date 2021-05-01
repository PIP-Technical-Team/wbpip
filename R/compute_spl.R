#' Compute societal poverty lines
#'
#' @param x data frame with either micro data, group data, or imputed data
#' @param welfare welfare vector
#' @param weight population weights vector
#'
#' @return scalar with poverty line
#' @export
#'
#' @examples
compute_spl <- function(x,...) {
  UseMethod("compute_spl", object = x)
}

#' @export
compute_spl.pipmd <- function(x,
                              welfare,
                              weight,
                              cpi = pipload::pip_load_aux("cpi"),
                              ppp = pipload::pip_load_aux("ppp")) {

  ppp <- ppp[ppp_default == TRUE]

  # Merge survey table with PPP (left join)
  x <- joyn::merge(x, ppp,
                    by         = c("country_code", "ppp_data_level"),
                    match_type = "m:1",
                    yvars      = 'ppp',
                    keep       = "left",
                    reportvar  = FALSE,
                    verbose    = FALSE)

  # Merge survey table with CPI (left join)
  x <- joyn::merge(x, cpi,
                  by         = c("country_code", "survey_year",
                                 "survey_acronym", "cpi_data_level"),
                  match_type = "m:1",
                  yvars      = 'cpi',
                  keep       = "left",
                  reportvar  = FALSE,
                  verbose    = FALSE)


  setnames(x, "welfare", "welfare_lcu")
  x[, welfare_ppp := wbpip::deflate_welfare_mean(
    welfare_mean = welfare_lcu,
    ppp          = ppp,
    cpi          = cpi)
  ]

  p50 <- compute_median(x, "welfare_ppp", "weight")
  spl  <- 1 + (p50 * .5)
  spl  <- ifelse(spl < 1.9, 1.9, spl)

  return(spl)

}

#' @export
compute_spl.pipgd <- function(x, welfare, weight, ...) {


}

