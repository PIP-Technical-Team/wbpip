#' get synthetic vector based on data level
#'
#' @param dt data frame with grouped data
#' @param pop data frame with population data
#' @param mean numeric: maned vector of means. The name of each value correspond
#'   the data level of the value
#' @param level charcter: data level. itcould  be nations, urban, rural, or any
#'   other subnational division
#'
#' @return data.frame
#' @export
get_synth_vector <- function(dt, pop, mean, level) {

  df <- dt[max_domain == level]
  ccode     <- dt[, unique(country_code)]
  svid_year <- dt[, unique(surveyid_year)]

  popf   <- pop[country_code     == ccode
                & year           == svid_year
                & pop_data_level == level,
                pop]

  wf <- sd_create_synth_vector(df$welfare,
                               df$weight,
                               mean = mean[level],
                               pop  = popf)
  return(wf)
}
