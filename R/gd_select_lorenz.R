#' Select best Lorenz fit
#'
#' Select best Lorenz fit and adjust the returned statistics if needed
#'
#' @param lq list: Results from Lorenz Quadratic functional form.
#' output of `gd_compute_pip_stats_lq()`
#' @param lb list: Results from Lorenz Beta functional form.
#' output of `gd_compute_pip_stats_lb()`
#'
#' @return list
#' @export
#'

gd_select_lorenz <- function(lq, lb) {

  datamean <- lq[["mean"]]

  is_valid <- lq[["is_valid"]] | lb[["is_valid"]]
  is_normal <- lq[["is_normal"]] | lb[["is_normal"]]

  # first the poverty selection
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

  use_GQz <- TRUE
  if (pov_flag %in% c(13, 9, 7, 5, 4, 1) ) {
    use_GQz <- FALSE
  } else if (pov_flag %in% c(15, 3)) {
    use_GQz <- lq[["ssez"]] <= lb[["ssez"]]
  }

  # Making the distributional selection
  #-----------------------------
  #  Qv  Bv dis_flag use
  #-----------------------------
  #  X   X    3     smaller SSEz
  #  X   O    2     Q
  #  O   X    1     B
  #  O   O    0     n.a.
  #-----------------------------

  use_GQ <- TRUE
  dis_flag <- ifelse(lq[["is_valid"]], 2, 0) +
    ifelse(lb[["is_valid"]], 1, 0)

  if (dis_flag == 3) {
    use_GQ <- lq[["sse"]] <= lb[["sse"]]
  } else if (dis_flag == 1) {
    use_GQ <- FALSE
  }

  if (is_valid) {
    if (use_GQ) {
      sse    <- lq[["sse"]]
      z_min  <- lq[["z_min"]]
      z_max  <- lq[["z_max"]]
      gini     <- lq[["gini"]]
      median <- lq[["median"]]
      pol    <- lq[["pol"]]
      # rmed   <- lq[["rmed # Returns NULL: Figure out what the issue is!!
      rmhalf <- lq[["rmhalf"]]
      ris    <- lq[["ris"]]

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
      pol <- lb[["pol"]]

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

  if (!is_normal) return(NA)
  if (use_GQz)
  {
    povline     <- lq[["povline"]]
    headcount   <- lq[["headcount"]]
    pg          <- lq[["pg"]]
    p2          <- lq[["p2"]]
    eh          <- lq[["eh"]]
    epg         <- lq[["epg"]]
    ep          <- lq[["ep"]]
    gh          <- lq[["gh"]]
    gpg         <- lq[["gpg"]]
    gp          <- lq[["gp"]]

    if (lq[["watt"]] >= 0) {
      watt <- lq[["watt"]]
    } else if (lb[["watt"]] >= 0) {
      watt <- lb[["watt"]]
    } else {
      watt <- NA
    }
  } else
  {
    povline     <- lb[["povline"]]
    headcount   <- lb[["headcount"]]
    pg          <- lb[["pg"]]
    p2          <- lb[["p2"]]
    eh          <- lb[["eh"]]
    epg         <- lb[["epg"]]
    ep          <- lb[["ep"]]
    gh          <- lb[["gh"]]
    gpg         <- lb[["gpg"]]
    gp          <- lb[["gp"]]

    if (lb[["watt"]] >= 0) {
      watt <- lb[["watt"]]
    } else if (lq[["watt"]] >= 0) {
      watt <- lq[["watt"]]
    } else {
      watt <- NA
    }
  }
  # fix abnormal values
  if (headcount < 0) {
    headcount <- NA
    pg        <- NA
    p2        <- NA
  }

  if (headcount > 1) {
    headcount  <- 0.99999
    pg         <- 0.99998
    p2         <- 0.99997
  }

  return(list(
    mean          = datamean,
    povline       = povline,
    z_min         = z_min,
    z_max         = z_max,
    # ppp           = lq[["ppp"]],
    gini          = gini,
    median        = median,
    # rmed          = rmed,
    rmhalf        = rmhalf,
    pol           = pol,
    ris           = ris,
    mld           = mld,
    dcm           = lq[["dcm"]],
    deciles       = deciles,
    headcount     = headcount,
    pg            = pg,
    p2            = p2,
    eh            = eh,
    epg           = epg,
    ep            = ep,
    gh            = gh,
    gpg           = gpg,
    gp            = gp,
    watt          = watt,
    sse           = sse
  ))

}
