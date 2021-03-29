#' Select best Lorenz fit
#' Version used in production
#'
#' Select best Lorenz fit and adjust the returned statistics if needed.
#'
#' @param lq list: Results from Lorenz Quadratic functional form. output of
#'   `gd_compute_pip_stats_lq()`.
#' @param lb list: Results from Lorenz Beta functional form. output of
#'   `gd_compute_pip_stats_lb()`.
#'
#' @return list
#' @keywords internal
prod_gd_select_lorenz <- function(lq, lb) {

  # Set default value
  datamean <- lq[["mean"]]
  is_valid <- lq[["is_valid"]] | lb[["is_valid"]]
  is_normal <- lq[["is_normal"]] | lb[["is_normal"]]

  # Selection of Lorenz fit for poverty statistics
  use_lq_for_pov <- use_lq_for_poverty(lq = lq,
                                       lb = lb)

  # Retrieve poverty statistics
  pov <- retrieve_poverty(lq = lq,
                          lb = lb,
                          is_normal = is_normal,
                          use_lq_for_pov = use_lq_for_pov)

  return(list(
    mean             = datamean,
    poverty_line     = pov[["poverty_line"]],
    dcm              = lq[["dcm"]],
    headcount        = pov[["headcount"]],
    poverty_gap      = pov[["poverty_gap"]],
    poverty_severity = pov[["poverty_severity"]],
    eh               = pov[["eh"]],
    epg              = pov[["epg"]],
    ep               = pov[["ep"]],
    gh               = pov[["gh"]],
    gpg              = pov[["gpg"]],
    gp               = pov[["gp"]],
    watts            = pov[["watts"]]
  ))

}
