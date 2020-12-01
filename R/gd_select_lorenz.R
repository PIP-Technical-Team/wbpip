#' Select best Lorenz fit
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
gd_select_lorenz <- function(lq, lb) {

  # Set default value
  datamean <- lq[["mean"]]
  is_valid <- lq[["is_valid"]] | lb[["is_valid"]]
  is_normal <- lq[["is_normal"]] | lb[["is_normal"]]

  # Selection of Lorenz fit for poverty statistics
  use_lq_for_pov <- use_lq_for_poverty(lq = lq,
                                       lb = lb)

  # Selection of Lorenz fit for distributional statistics
  use_lq_for_dist <- use_lq_for_distributional(lq = lq,
                                               lb = lb)

  # Retrieve distributional statistics
  dist <- retrieve_distributional(lq = lq,
                                  lb = lb,
                                  is_valid = is_valid,
                                  use_lq_for_dist = use_lq_for_dist)

  # Retrieve poverty statistics
  pov <- retrieve_poverty(lq = lq,
                          lb = lb,
                          is_normal = is_normal,
                          use_lq_for_pov = use_lq_for_pov)

  return(list(
    mean             = datamean,
    poverty_line     = pov[["poverty_line"]],
    z_min            = dist[["z_min"]],
    z_max            = dist[["z_max"]],
    # ppp            = lq[["ppp"]],
    gini             = dist[["gini"]],
    median           = dist[["median"]],
    # rmed           = rmed,
    rmhalf           = dist[["rmhalf"]],
    polarization     = dist[["polarization"]],
    ris              = dist[["ris"]],
    mld              = dist[["mld"]],
    dcm              = lq[["dcm"]],
    deciles          = dist[["deciles"]],
    headcount        = pov[["headcount"]],
    poverty_gap      = pov[["poverty_gap"]],
    poverty_severity = pov[["poverty_severity"]],
    eh               = pov[["eh"]],
    epg              = pov[["epg"]],
    ep               = pov[["ep"]],
    gh               = pov[["gh"]],
    gpg              = pov[["gpg"]],
    gp               = pov[["gp"]],
    watts            = pov[["watts"]],
    sse              = dist[["sse"]]
  ))

}


#' Algorithm to decide which Lorenz fit to use for poverty statistics
#'
#'
#' @inheritParams gd_select_lorenz
#'
#' @return logical:
#' returns TRUE for Lorenz Quadratic
#' returns FALSE for Lorenz Beta
#' @keywords internal

use_lq_for_poverty <- function(lq,
                               lb) {

  # Rules to be applied for Lorenz functional form selection ----------------

  # X = Yes
  # O = No
  # Qv = Is Lorenz Quadratic fit valid?
  # Bv = Is Lorenz Beta fit valid?
  # Qc = Is Lorenz Quadratic fit normal?
  # Bc = Is Lorenz Beta fit normal?
  # SSEz = Sum of Squared Error up to the poverty line

  #-----------------------------
  #  Qv  Bv  Qc  Bc pov_flag use
  #-----------------------------
  #  X   X   X   X   15     smaller SSEz
  #  X   X   X   O   14     Q
  #  X   X   O   X   13     B
  #  X   X   O   O   12     Q
  #-----------------------------
  #  X   O   X   X   11     Q
  #  X   O   X   O   10     Q
  #  X   O   O   X    9     B
  #  X   O   O   O    8     Q
  #-----------------------------
  #  O   X   X   X    7     B
  #  O   X   X   O    6     Q
  #  O   X   O   X    5     B
  #  O   X   O   O    4     B
  #-----------------------------
  #  O   O   X   X    3     smaller SSEz
  #  O   O   X   O    2     Q
  #  O   O   O   X    1     B
  #  O   O   O   O    0     Q
  #-----------------------------

  pov_flag <- ifelse(lq[["is_valid"]], 8, 0) +
    ifelse(lb[["is_valid"]], 4, 0) +
    ifelse(lq[["is_normal"]], 2, 0) +
    ifelse(lb[["is_normal"]], 1, 0)

  use_lq_for_pov <- TRUE
  if (pov_flag %in% c(13, 9, 7, 5, 4, 1) ) {
    use_lq_for_pov <- FALSE
  } else if (pov_flag %in% c(15, 3)) {
    use_lq_for_pov <- lq[["ssez"]] <= lb[["ssez"]]
  }

  return(
    use_lq_for_pov
  )
}

#' Algorithm to decide which Lorenz fit to use for distributional statistics
#'
#'
#' @inheritParams gd_select_lorenz
#'
#' @return logical:
#' returns TRUE for Lorenz Quadratic
#' returns FALSE for Lorenz Beta
#' @keywords internal
use_lq_for_distributional <- function(lq,
                                      lb) {
  # X = Yes
  # O = No
  # Qv = Is Lorenz Quadratic fit valid?
  # Bv = Is Lorenz Beta fit valid?

  # Making the distributional selection
  #-----------------------------
  #  Qv  Bv dis_flag use
  #-----------------------------
  #  X   X    3     smaller SSEz
  #  X   O    2     Q
  #  O   X    1     B
  #  O   O    0     n.a.
  #-----------------------------

  use_lq_for_dist <- TRUE
  dis_flag <- ifelse(lq[["is_valid"]], 2, 0) +
    ifelse(lb[["is_valid"]], 1, 0)

  if (dis_flag == 3) {
    use_lq_for_dist <- lq[["sse"]] <= lb[["sse"]]
  } else if (dis_flag == 1) {
    use_lq_for_dist <- FALSE
  }

  return(
    use_lq_for_dist
  )
}

#' Algorithm to retrieve correct distributional statistics
#'
#'
#' @inheritParams gd_select_lorenz
#' @param is_valid logical: Whether at least one of the Lorenz fit is valid
#' @param is_lq_for_dist logical: Whether to use LQ (TRUE) or Beta fit (FALSE)
#'
#' @return list
#'
#' @keywords internal
retrieve_distributional <- function(lq,
                                    lb,
                                    is_valid,
                                    use_lq_for_dist) {

  if (is_valid) {
    if (use_lq_for_dist) {
      sse             <- lq[["sse"]]
      z_min           <- lq[["z_min"]]
      z_max           <- lq[["z_max"]]
      gini            <- lq[["gini"]]
      median          <- lq[["median"]]
      polarization    <- lq[["polarization"]]
      # rmed          <- lq[["rmed # Returns NULL: Figure out what the issue is!!
      rmhalf          <- lq[["rmhalf"]]
      ris             <- lq[["ris"]]

      deciles <- lq[["deciles"]]
      if (lq[["mld"]] >= 0) {
        mld <- lq[["mld"]]
      } else if (lb[["mld"]] >= 0) {
        mld <- lb[["mld"]]
      } else {
        mld <- NA
      }
    } else {
      sse    <- lb[["sse"]]
      z_min  <- lb[["z_min"]]
      z_max  <- lb[["z_max"]]
      gini   <- lb[["gini"]]
      median <- lb[["median"]]
      # rmed   <- lb[["rmed"]] # Returns NULL: Figure out what the issue is!!
      rmhalf <- lb[["rmhalf"]]
      ris    <- lb[["ris"]]

      deciles <- lb[["deciles"]]
      polarization <- lb[["polarization"]]

      if (lb[["mld"]] >= 0) {
        mld <- lb[["mld"]]
      } else if (lq[["mld"]] >= 0) {
        mld <- lq[["mld"]]
      } else {
        mld <- NA
      }
    }
  } else {
    for (i in seq_along(deciles)) {deciles[i] <- NA}
  }

  return(
    z_min            = z_min,
    z_max            = z_max,
    gini             = gini,
    median           = median,
    # rmed           = rmed,
    rmhalf           = rmhalf,
    polarization     = polarization,
    ris              = ris,
    mld              = mld,
    deciles          = deciles,
    sse              = sse
  )
}

#' Algorithm to retrieve correct poverty statistics
#'
#'
#' @inheritParams gd_select_lorenz
#' @param is_normal logical: Whether at least one of the Lorenz fit is normal
#' @param is_lq_for_pov logical: Whether to use LQ (TRUE) or Beta fit (FALSE)
#'
#' @return list
#'
#' @keywords internal
#'
retrieve_poverty <- function(lq,
                             lb,
                             is_normal,
                             use_lq_for_pov) {

  if (!is_normal) return(NA)
  if (use_lq_for_pov)
  {
    poverty_line     <- lq[["poverty_line"]]
    headcount        <- lq[["headcount"]]
    poverty_gap      <- lq[["poverty_gap"]]
    poverty_severity <- lq[["poverty_severity"]]
    eh               <- lq[["eh"]]
    epg              <- lq[["epg"]]
    ep               <- lq[["ep"]]
    gh               <- lq[["gh"]]
    gpg              <- lq[["gpg"]]
    gp               <- lq[["gp"]]

    if (lq[["watts"]] >= 0) {
      watts <- lq[["watts"]]
    } else if (lb[["watts"]] >= 0) {
      watts <- lb[["watts"]]
    } else {
      watts <- NA
    }
  } else
  {
    poverty_line     <- lb[["poverty_line"]]
    headcount        <- lb[["headcount"]]
    poverty_gap      <- lb[["poverty_gap"]]
    poverty_severity <- lb[["poverty_severity"]]
    eh               <- lb[["eh"]]
    epg              <- lb[["epg"]]
    ep               <- lb[["ep"]]
    gh               <- lb[["gh"]]
    gpg              <- lb[["gpg"]]
    gp               <- lb[["gp"]]

    if (lb[["watts"]] >= 0) {
      watts <- lb[["watts"]]
    } else if (lq[["watts"]] >= 0) {
      watts <- lq[["watts"]]
    } else {
      watts <- NA
    }
  }
  # fix abnormal values
  if (headcount < 0) {
    headcount        <- NA
    poverty_gap      <- NA
    poverty_severity <- NA
  }

  if (headcount > 1) {
    headcount        <- 0.99999
    poverty_gap      <- 0.99998
    poverty_severity <- 0.99997
  }

  return(list(
    poverty_line     = poverty_line,
    headcount        = headcount,
    poverty_gap      = poverty_gap,
    poverty_severity = poverty_severity,
    eh               = eh,
    epg              = epg,
    ep               = ep,
    gh               = gh,
    gpg              = gpg,
    gp               = gp,
    watts            = watts
  ))

}
