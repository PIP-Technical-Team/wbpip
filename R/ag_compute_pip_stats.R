#' Computes poverty statistics (aggregated)
#'
#' Compute poverty statistics for aggregated data distribution.
#'
#' @inheritParams gd_compute_pip_stats
#' @param area character: Area (Urban or Rural)
#' @param area_pop numeric: Total population per area.
#' @return list
#' @keywords internal
ag_compute_pip_stats <- function(welfare,
                                 povline,
                                 population,
                                 area,
                                 area_pop,
                                 requested_mean,
                                 popshare = NULL,
                                 default_ppp = NULL,
                                 ppp = NULL,
                                 p0 = 0.5) {

  assertthat::assert_that(assertthat::are_equal(sort(unique(area)),
                                                sort(names(area_pop))))
  # Compute stats for each sub-group
  out <- vector(mode = "list", length = length(area_pop))
  for (i in seq_along(area_pop)) {
    area_name <- names(area_pop)[i]
    tmp_welfare <- welfare[area == area_name]
    tmp_population <- population[area == area_name]
    tmp_mean <- requested_mean[[area_name]]
    tmp_ppp <- default_ppp[[area_name]]

    out[[i]] <- gd_compute_poverty_stats(welfare = tmp_welfare,
                                         povline = povline,
                                         population = tmp_population,
                                         requested_mean = tmp_mean,
                                         default_ppp = tmp_ppp
    )
    names(out)[i] <- area_name

  }

  # Compute population weighted average
  wgt_urban <- area_pop[["urban"]] / sum(unlist(area_pop))
  wgt_rural <- 1 - wgt_urban

  out_mean = wgt_urban * requested_mean[["urban"]] + wgt_rural * requested_mean[["rural"]]

  if (out[["rural"]]$poverty_severity < 0) # Check if rural poverty severity < 0
  {
    if (out[["urban"]]$poverty_severity < 0) # Same for urban
    {
      out_headcount <- out_poverty_gap <- out_poverty_severity <- NA
    }
    else
    {
      out_headcount        <- out[["urban"]]$headcount
      out_poverty_gap      <- out[["urban"]]$poverty_gap
      out_poverty_severity <- out[["urban"]]$poverty_severity
    }
  }
  else
  {
    if (out[["urban"]]$poverty_severity < 0)
    {
      out_headcount        <- out[["rural"]]$headcount
      out_poverty_gap      <- out[["rural"]]$poverty_gap
      out_poverty_severity <- out[["rural"]]$poverty_severity
    }
    else
    {
      out_headcount <- wgt_rural * out[["rural"]]$headcount +
        wgt_urban * out[["urban"]]$headcount

      out_poverty_gap <- wgt_rural * out[["rural"]]$poverty_gap +
        wgt_urban * out[["urban"]]$poverty_gap

      out_poverty_severity <- wgt_rural * out[["rural"]]$poverty_severity +
        wgt_urban * out[["urban"]]$poverty_severity
    }
  }

  if (out[["urban"]]$watts > 0 & out[["rural"]]$watts > 0) {
    out_watts <- wgt_rural * out[["rural"]]$watts +
      wgt_urban * out[["urban"]]$watts
  } else {
    out_watts <- NA
  }

  return(list(
    poverty_line = povline,
    mean = out_mean,
    median = NA,
    headcount = out_headcount,
    poverty_gap = out_poverty_gap,
    poverty_severity = out_poverty_severity,
    watts = out_watts,
    gini = NA,
    mld = NA,
    polarization = NA,
    deciles = rep(NA, 10)
  ))
}
