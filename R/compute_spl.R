#' Compute societal poverty lines
#'
#' @param dt data frame with either micro data, group data, or imputed data
#' @param welfare character: name of variable with welfare vector
#' @param weight character: name of variable with population (weight) vector
#'
#' @return scalar with poverty line
#' @export
#'
#' @examples
compute_spl <- function(dt,...) {
  UseMethod("compute_spl", object = dt)
}


#' Compute societal poverty lines for microdata in PIP
#'
#' @param dt
#' @inheritParams compute_spl
#' @param cpi data frame with CPI data. Default `pipload::pip_load_aux("cpi")`
#' @param ppp data frame with PPP data. Default `pipload::pip_load_aux("ppp")`
#'
#' @export
compute_spl.pipmd <- function(dt,
                              welfare,
                              weight,
                              cpi = pipload::pip_load_aux("cpi"),
                              ppp = pipload::pip_load_aux("ppp")) {

  dt <- join_cpi_ppp(dt, cpi, ppp)

  setnames(dt, welfare, "welfare_lcu")
  dt[, welfare_ppp := wbpip::deflate_welfare_mean(
    welfare_mean = welfare_lcu,
    ppp          = ppp,
    cpi          = cpi)
  ]


  p50 <- compute_median(dt, "welfare_ppp", weight)
  spl  <- purrr::map(.x = p50,
                     .f = ~{
                       s <- 1 + (.x * .5)
                       s  <- ifelse(s < 1.9, 1.9, s)
                     })

  return(spl)

}

#' Compute societal poverty lines for grouped data in PIP
#'
#' @param dt
#' @inheritParams compute_spl
#' @param cpi data frame with CPI data. Default `pipload::pip_load_aux("cpi")`
#' @param ppp data frame with PPP data. Default `pipload::pip_load_aux("ppp")`
#'
#' @export
compute_spl.pipgd <- function(dt,
                              welfare,
                              weight,
                              mean,
                              cpi = pipload::pip_load_aux("cpi"),
                              ppp = pipload::pip_load_aux("ppp"),
                              pop = NULL) {

  # deflators
  df <- join_cpi_ppp(dt, cpi, ppp)

  # get info
  dti <- get_dt_info(dt)

  df <- df[,
           lapply(.SD, unique),
           .SDcols = c("cpi", "ppp"),
           by = "max_domain"]

  p50 <- compute_median(dt,
                        welfare = welfare,
                        weight  = weight,
                        mean    = mean,
                        pop     = pop)

  if (dti$data_level != "D1") { # Urban/rural or subnat level

    p50 <- p50[!names(p50) %in% "national"]

  }


  dd <- tibble::enframe(p50, name = "max_domain")
  dd <- tidyr::unnest(dd,
                      cols = value)

  ndt <- joyn::merge(dd, df,
                     by = "max_domain",
                     match_type = "1:1",
                     reportvar  = FALSE,
                     verbose    = FALSE)

  setnames(ndt, "value", "median_lcu")
  ndt[, median_ppp := wbpip::deflate_welfare_mean(
    welfare_mean = median_lcu,
    ppp          = ppp,
    cpi          = cpi)
    ][,
      spl := 1 + (median_ppp * .5)
      ][,
        spl := fifelse(spl < 1.9, 1.9, spl)
        ]

  res <- vector(mode = "list",
                length = nrow(ndt))

  for (i in seq_along(res)) {

    res[i] <- ndt[i, median_ppp]

  }

  names(res) <- ndt[, max_domain]
  return(res)

}


#' @keywords internal
join_cpi_ppp <- function(dt, cpi, ppp) {
  ppp <- ppp[ppp_default == TRUE]

  # Merge survey table with PPP (left join)
  dt <- joyn::merge(dt, ppp,
                    by         = c("country_code", "ppp_data_level"),
                    match_type = "m:1",
                    yvars      = 'ppp',
                    keep       = "left",
                    reportvar  = FALSE,
                    verbose    = FALSE)

  # Merge survey table with CPI (left join)
  dt <- joyn::merge(dt, cpi,
                    by         = c("country_code", "survey_year",
                                   "survey_acronym", "cpi_data_level"),
                    match_type = "m:1",
                    yvars      = 'cpi',
                    keep       = "left",
                    reportvar  = FALSE,
                    verbose    = FALSE)

  return(dt)
}
