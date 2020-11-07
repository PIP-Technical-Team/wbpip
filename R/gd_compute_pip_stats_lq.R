#' Computes poverty statistics from grouped data
#'
#' @param population numeric: population vector whose form depends on on `type`.
#' @param welfare numeric: welfare vector whose form depends on on `type`.
#' @param type numeric: Type of data. if `type = 1`, `population` must be the
#' cumulative proportion of population and `welfare` must be the cumulative
#' proportion of income held by that proportion of the population (Lorenz Curve).
#' if `type = 2`, `population` must be the proportion of population and
#' `welfare` must be the proportion of income.
#' If `type = 5`, then `population` must be the Percentage
#' of the population in a given interval of incomes, whereas `welfare` must be
#' the mean income of that interval. Default is 1.
#' @param mean numeric: Welfare mean if `type = 1` or `type = 2`
#' @param povline numeric: Poverty line
#' @param ppp numeric: PPP request by user
#' @param default_ppp numeric: Default purchasing power parity

#' @param popshare numeric: Share of population living below the poverty line.
#' Optional
#' @param is_lq boolean: indicates whether the estimation of lorenz quadratic
#'
#' @return list
#' @export
#'
#' @examples
#' # gd_estimate_lq(.data, , povline, default_ppp, ppp, popshare, p, l, is_lq)
gd_compute_pip_stats_lq <- function(population,
                                    welfare,
                                    mean,
                                    default_ppp,
                                    type     = 1,
                                    povline  = NULL,
                                    ppp      = NULL,
                                    popshare = NULL,
                                    is_lq    = TRUE,
                                    p0       = 0.5) {

  check_input_gd_compute_pip_stats_lq(population,
                                      welfare,
                                      mean,
                                      default_ppp,
                                      type,
                                      povline,
                                      ppp,
                                      popshare,
                                      is_lq,
                                      p0)

  #--------- convert type 5 to type 1 ---------
  if (type %in% c(3, 5)) {

    mean    <- weighted.mean(welfare, population)

    X       <- population*welfare
    X       <- cumsum(X)
    welfare <- X/max(X)

    population <- cumsum(population)

  }


  #--------- Conditions ---------


  if (!is.null(ppp)) {

    mean <- mean * default_ppp / ppp

  } else {

    ppp <- default_ppp

  }

  if (   ( is.null(povline) &&  is.null(popshare))
      || (!is.null(povline) && !is.null(popshare)) ) {
    msg     <- "Either `povline` or `popshare` most be provided"
    rlang::abort(msg,
                 class = "wbpip_error")
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #--------- functional form and Estimate
  #    regression coefficients using LQ parametrization ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  # STEP 1: Prep data to fit functional form
  reg_results <- create_functional_form_lq(population = population,
                                            welfare    = welfare)

  reg_coef <- reg_results$coef_stat$estimate


  # What is is_lq? Where is it defined?
  if (is_lq) {
    A <- reg_coef[1]
  } else {
    A <- exp(reg_coef[1]) # Why exp() if is_lq == FALSE?
  }

  B <- reg_coef[2]
  C <- reg_coef[3]


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #-----   components of poverty measures using parameters ------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




  # OPTIONAL: Only when popshare is supplied
  # return poverty line if share of population living in poverty is supplied
  # intead of a poverty line
  if (!is.null(popshare)) {
    povline <- derive_lq(popshare, A, B, C) * mean
  }

  # Boundary conditions (Why 4?)
  z_min <- mean * derive_lq(0.001, A, B, C) + 4
  z_max <- mean * derive_lq(0.980, A, B, C) - 4
  z_min <- ifelse(z_min < 0, 0, z_min)

  results1 <- list(mean, povline, z_min, z_max, ppp)
  names(results1) <- list("mean", "povline", "z_min", "z_max", "ppp")

  # STEP 3: Estimate poverty measures based on identified parameters
  results2 <- gd_estimate_lq(n_obs, mean, povline, p0, A, B, C)

  # STEP 4: Compute measure of regression fit
  results_fit <- gd_compute_fit_lq(welfare, population, results2$headcount, A, B, C)

  res <- c(results1, results2, results_fit, reg_results)

  return(res)

}

#' Returns the first derivative of the Lorenz quadratic
#'
#' `derive_lq()` returns the first derivative of the quadratic Lorenz curves
#' with c = 1. General quadratic form: ax^2 + bxy + cy^2 + dx + ey + f = 0
#'
#' @param x numeric: point on curve
#' @param A numeric vector: lorenz curve coefficient
#' @param B numeric vector: lorenz curve coefficient
#' @param C numeric vector: lorenz curve coefficient
#'
#' @return numeric
#'
derive_lq <- function(x, A, B, C) {
  e <- -(A + B + C + 1)
  m <- (B^2) - (4 * A)
  n <- (2 * B * e) - (4 * C)
  tmp <- (m * x^2) + (n * x) + (e^2)
  tmp <- ifelse(tmp < 0, 0, tmp)

  # Formula for first derivative of GQ Lorenz Curve
  val <- -(0.5 * B) - (0.25 * (2 * m * x + n) / sqrt(tmp))

  return(val)
}

#' Check validity of Lorenz Quadratic fit
#'
#' `check_curve_validity_lq()` checks the validity of the Lorenz Quadratic fit
#'
#' @param A numeric: First regression coefficient
#' @param B numeric: Second regression coefficient
#' @param C numeric: Third regression coefficient
#' @param e numeric: e = -(A + B + C + 1): condition for the curve to go through
#' (1, 1)
#' @param m numeric: m = (B^2) - (4 * A). m < 0: condition for the curve to be
#' an ellipse (m is called alpha in paper)
#' @param n numeric: n = (2 * B * e) - (4 * C). n is called Beta in paper
#' @param r r = (n^2) - (4 * m * e^2). r is called K in paper
#'
#' @return list
#'
#' @seealso \href{https://EconPapers.repec.org/RePEc:eee:econom:v:40:y:1989:i:2:p:327-338}{Original quadratic Lorenz curve paper}
#' @seealso \href{https://www.sciencedirect.com/science/article/abs/pii/S0304407613000158?via%3Dihub}{Corrigendum to Elliptical Lorenz Curves}
#' @seealso \href{https://www.ifpri.org/cdmref/p15738coll2/id/125673}{
#' Computational Tools For Poverty Measurement And Analysis}
#'
check_curve_validity_lq <- function(A, B, C, e, m, n, r) {

  is_normal <- FALSE
  is_valid <- FALSE

  # r needs to be > 0 because need to extract sq root
  if (r < 0) {return(list(is_normal = is_normal,
                          is_valid = is_valid))}

  if (e > 0 || C < 0) {
    return(list(is_normal = is_normal,
                is_valid = is_valid))
  }

  # Failure conditions for checking theoretically valid Lorenz curve
  # Found in section 4 of Datt computational tools paper
  cn1 <- n^2
  cn3 <- cn1 / (4 * e^2)

  if (!((m < 0) |
        ((m > 0) & (m < cn3) & (n >= 0)) |
        ((m > 0) & (m < -n/2) & (m < cn3)))) {
    return(list(is_normal = is_normal,
                is_valid = is_valid))
  }

  is_normal <- TRUE
  is_valid <- (A + C) >= 0.9

  return(list(is_normal = is_normal,
              is_valid = is_valid))

}

#' Compute gini index from Lorenz Quadratic fit
#'
#' `gd_compute_gini_lq()` computes the gini index from a Lorenz Quadratic fit
#'
#' @param A numeric: First regression coefficient
#' @param B numeric: Second regression coefficient
#' @param C numeric: Third regression coefficient
#' @param e numeric: e = -(A + B + C + 1): condition for the curve to go through
#' (1, 1)
#' @param m numeric: m = (B^2) - (4 * A). m < 0: condition for the curve to be
#' an ellipse (m is called alpha in paper)
#' @param n numeric: n = (2 * B * e) - (4 * C). n is called Beta in paper
#' @param r numeric: r = (n^2) - (4 * m * e^2). r is called K in paper
#'
#' @return numeric
#'
#' @seealso \href{https://www.ifpri.org/cdmref/p15738coll2/id/125673}{
#' Computational Tools For Poverty Measurement And Analysis}
gd_compute_gini_lq <- function(A, B, C, e, m, n, r) {

  # For the GQ Lorenz curve, the Gini formula are valid under the condition A+C>=1
  # P.isValid <- (A + C) >= 0.9
  # P.isNormal <- TRUE

  e1 <- abs(A + C - 1)
  e2 <- 1 + (B / 2) + e

  tmp1 <- n * (B + 2) / (4 * m)
  tmp2 <- (r^2) / (8 * m)
  tmp3 <- (2 * m) + n

  if (m > 0) {
    # tmpnum <- tmp3 + 2 * sqrt(m) * abs(e)
    # tmpden <- n - 2 * abs(e) * sqrt(m)

    # Formula from Datt paper
    # CHECK that code matches formulas in paper
    gini <- e2 + (tmp3/(4 * m)) * e1 - (n*abs(e) / (4 * m)) - ((r^2) / (8 * sqrt(m))) * log(abs(((tmp3 + (2 * sqrt(m) * e1)))/(n + (2 * sqrt(m) * abs(e)))))
    #P.gi <- (e/2) - tmp1 - (tmp2 * log(abs(tmpnum/tmpden)) / sqrt(m))

  } else {
    tmp4 <- ((2*m) + n) / r
    tmp4 <- ifelse(tmp4 < -1, -1, tmp4)
    tmp4 <- ifelse(tmp4 > 1, 1, tmp4)

    # Formula does not match with paper
    gini <- e2 + (tmp3/(4*m)) * e1 - (n*abs(e)/(4*m)) + (tmp2 * (asin(tmp4) - asin(n/r)) / sqrt(-m))
    # P.gi <- (e/2) - tmp1 + ((tmp2 * (asin(tmp4) - asin(n/r))) / sqrt(-m))
  }

  return(gini)

}

#' Solves for quadratic Lorenz curves
#'
#' `value_at_lq()`solves for quadratic Lorenz curves with c = 1
#' General quadratic form: ax^2 + bxy + cy^2 + dx + ey + f = 0
#'
#' @param x numeric: point on curve
#' @param A numeric vector: First lorenz curve coefficient
#' @param B numeric vector: Second lorenz curve coefficient
#' @param C numeric vector: Third lorenz curve coefficient
#'
#' @return numeric
#'
value_at_lq <- function(x, A, B, C) {
  e <- -(A + B + C + 1)
  m <- (B^2) - (4 * A)
  n <- (2 * B * e) - (4 * C)
  temp <- (m * x^2) + (n * x) + (e^2)
  temp <- ifelse(temp < 0, 0, temp)

  # Solving the equation of the Lorenz curve
  estle <- -0.5 * ((B * x) + e + sqrt(temp))

  return(estle)
}

#' Computes MLD from Lorenz Quadratic fit
#'
#' `gd_compute_mld_lq()` computes the Mean Log deviation (MLD) from a Lorenz
#' Quadratic fit
#'
#' @param dd numeric
#' @param A numeric vector: lorenz curve coefficient
#' @param B numeric vector: lorenz curve coefficient
#' @param C numeric vector: lorenz curve coefficient
#'
#' @return numeric
#'
gd_compute_mld_lq <- function(dd, A, B, C) {
  x1 <- derive_lq(0.0005, A, B, C)
  gap <- 0
  mld <- 0
  if (x1 == 0) {
    gap <- 0.0005
  }
  else {
    mld <- log(x1) * 0.001
  }
  x1 <- derive_lq(0, A, B, C)
  for (xstep in seq(0, 0.998, 0.001)) {
    x2 <- derive_lq(xstep + 0.001, A, B, C)
    if ((x1 <= 0) || (x2 <= 0)) {
      gap <- gap + 0.001
      if (gap > 0.5) {
        return(-1)
      }
    }
    else {
      gap <- 0
      mld <- mld + (log(x1) + log(x2)) * 0.0005
    }
    x1 <- x2
  }
  return(-mld)
}

#' Compute quantiles from Lorenz Quandratic fit
#'
#' `gd_compute_quantile_lq()` computes quantiles from a Lorenz Quadratic fit.
#'
#' @param A numeric vector: lorenz curve coefficient
#' @param B numeric vector: lorenz curve coefficient
#' @param C numeric vector: lorenz curve coefficient
#'
#' @return numeric
#'
gd_compute_quantile_lq <- function(A, B, C, n_quantile = 10) {
  vec <- vector(mode = "numeric", length = n_quantile)
  x1 <- 1 / n_quantile
  q <- 0
  lastq <- 0
  for (i in seq_len(n_quantile - 1)) {
    q <- value_at_lq(x1, A, B, C)
    v <- q - lastq
    vec[i] <- v
    lastq <- q
    x1 <- x1 + 1 / n_quantile
  }
  vec[n_quantile] <- 1 - lastq

  return(vec)
}

#'  Computes Watts Index from Quadratic Lorenz fit
#'
#' `gd_compute_watts_lq()` computes Watts Index from Quadratic Lorenz fit
#' The first distribution-sensitive poverty measure was proposed in 1968 by Watts
#' It is defined as the mean across the population of the proportionate poverty
#' gaps, as measured by the log of the ratio of the poverty line to income,
#' where the mean is formed over the whole population, counting the nonpoor as
#' having a zero poverty gap.
#'
#' @param headcount numeric: headcount index
#' @param mu numeric
#' @param povline numeric: poverty line
#' @param dd numeric
#' @param A numeric vector: lorenz curve coefficient
#' @param B numeric vector: lorenz curve coefficient
#' @param C numeric vector: lorenz curve coefficient
#'
#' @return numeric
#' @export
#'
#' @examples
#' watt_index_lq(headcount, dd, A, B, C)
#'
gd_compute_watts_lq <- function(headcount, mu, povline, dd, A, B, C) {
  if (headcount <= 0) {
    return(0)
  }

  # x1 = x2 = xstep = xend = gap <- 0
  x1 <- 0
  x2 <- 0
  xstep <- 0
  xend <- 0
  gap <- 0
  snw <- headcount * dd
  watts <- 0

  x1 <- derive_lq(snw / 2, A, B, C)
  if (x1 <= 0) {
    gap <- snw / 2
  } else {
    watts <- log(x1) * snw
  }
  xend <- headcount - snw
  x1 <- derive_lq(0, A, B, C)
  # Number of steps seems to be different from what happens in .Net codebase
  for (xstep in seq(0, xend, by = snw)) {
    x2 <- derive_lq(xstep + snw, A, B, C)
    if ((x1 <= 0) || (x2 <= 0)) {
      gap <- gap + snw
      if (gap > 0.05) {
        return(NA)
      }
    } else {
      gap <- 0
      watts <- watts + (log(x1) + log(x2)) * snw * 0.5
    }
    x1 <- x2
  }
  if ((mu != 0) && (watts != 0)) {
    x1 <- povline / mu
    if (x1 > 0) {
      watts <- log(x1) * headcount - watts
      if (watts > 0) {
        return(watts)
      }
    }
    return(NA)
  }
}

#' Computes polarization index from Quadratic Lorenz fit
#'
#' @param mean numeric: Welfare mean
#' @param p0 numeric: To document
#' @param dcm numeric: To document
#' @param A numeric: Lorenz curve coefficient
#' @param B numeric: Lorenz curve coefficient
#' @param C numeric: Lorenz curve coefficient
#'
#' @return numeric
#'
gd_compute_polarization_lq <- function(mean,
                                       p0,
                                       dcm,
                                       A, B, C) {

  pol <- 2 - (1 / p0) +
    (dcm - (2 * value_at_lq(p0, A, B, C) * mean)) /
    (p0 * mean * derive_lq(p0, A, B, C))

  return(pol)
}

#' Computes distributional stats from Lorenz Quadratic fit
#'
#' @param mean numeric: welfare mean
#' @param p0 numeric: To document
#' @param A numeric: First regression coefficient
#' @param B numeric: Second regression coefficient
#' @param C numeric: Third regression coefficient
#' @param e numeric: e = -(A + B + C + 1): condition for the curve to go through
#' (1, 1)
#' @param m numeric: m = (B^2) - (4 * A). m < 0: condition for the curve to be
#' an ellipse (m is called alpha in paper)
#' @param n numeric: n = (2 * B * e) - (4 * C). n is called Beta in paper
#' @param r numeric:r = (n^2) - (4 * m * e^2). r is called K in paper
#'
#' @return list
#'
gd_compute_dist_stats_lq <- function(mean, p0, A, B, C, e, m, n, r) {

  gini    <- gd_compute_gini_lq(A, B, C, e, m, n, r)
  median  <- mean * derive_lq(0.5, A, B, C)
  rmhalf  <- value_at_lq(p0, A, B, C) * mean / p0 # What is this??
  dcm     <- (1 - gini) * mean
  pol     <- gd_compute_polarization_lq(mean, p0, dcm, A, B, C)
  ris     <- value_at_lq(0.5, A, B, C)
  mld     <- gd_compute_mld_lq(0.01, A, B, C)
  deciles <- gd_compute_quantile_lq(A, B, C)

  return(list(
    gini         = gini,
    median       = median,
    rmhalf       = rmhalf,
    dcm          = dcm,
    polarization = pol,
    ris          = ris,
    mld          = mld,
    deciles      = deciles
  ))
}

#' Computes poverty stats from Lorenz Quadratic fit
#'
#' @param mean numeric: welfare mean
#' @param povline numeric: Poverty line
#' @param A numeric: First regression coefficient
#' @param B numeric: Second regression coefficient
#' @param C numeric: Third regression coefficient
#' @param e numeric: e = -(A + B + C + 1): condition for the curve to go through
#' (1, 1)
#' @param m numeric: m = (B^2) - (4 * A). m < 0: condition for the curve to be
#' an ellipse (m is called alpha in paper)
#' @param n numeric: n = (2 * B * e) - (4 * C). n is called Beta in paper
#' @param r numeric:r = (n^2) - (4 * m * e^2). r is called K in paper
#' @param s1 numeric: To document
#'
#' @return list
#'
gd_compute_poverty_stats_lq <- function(mean,
                                        povline,
                                        A,
                                        B,
                                        C,
                                        e,
                                        m,
                                        n,
                                        r,
                                        s1,
                                        s2) {
  # Compute headcount
  bu <- B + (2 * povline / mean)
  u <- mean / povline

  headcount <- -(n + ((r * bu) / sqrt(bu^2 - m))) / (2 * m)

  tmp0 <- (m * headcount^2) + (n * headcount) + (e^2)
  tmp0 <- ifelse(tmp0 < 0, 0, tmp0)
  tmp0 <- sqrt(tmp0)

  # First derivative of the Lorenz curve
  dl <- -(0.5 * B) - (0.25 * ((2 * m * headcount) + n) / tmp0)

  # Second derivative of the Lorenz curve
  ddl <- r^2 / (tmp0^3 * 8)

  if (headcount < 0) {
    headcount = pov_gap = pov_gap_sq = watt <- 0
    eh = epg = ep = gh = gpg = gp  <- 0
  } else {

    # Poverty gap index (P.pg)
    pov_gap <- headcount - (u * value_at_lq(headcount, A, B, C))

    # P.p2 - Distributionally sensitive FGT poverty measure
    #P.p2 <- (2*P.pg) - P.h - u^2 * (A*P.h + B*value_at_lq(P.h, A, B, C) - (r/16 *log((1 - P.h/s1))/(1 - P.h/s2)))
    # Poverty severity
    pov_gap_sq <- (2 * pov_gap) - headcount -
      (u^2 * (A * headcount + B * value_at_lq(headcount, A, B, C) -
                ((r / 16) * log((1 - headcount / s1)/(1 - headcount / s2)))))

    # Elasticity of headcount index w.r.t mean (P.eh)
    eh <- -povline / (mean * headcount * ddl)

    # Elasticity of poverty gap index w.r.t mean (P.epg)
    epg <- 1 - (headcount / pov_gap)

    # Elasticity of distributionally sensitive FGT poverty measure w.r.t mean (P.ep)
    ep <- 2 * (1 - pov_gap / pov_gap_sq)

    # PElasticity of headcount index w.r.t gini index (P.gh)
    gh <- (1 - povline / mean) / (headcount * ddl)

    # Elasticity of poverty gap index w.r.t gini index (P.gpg)
    gpg <- 1 + (((mean / povline) - 1) * headcount / pov_gap)

    # Elasticity of distributionally sensitive FGT poverty measure w.r.t gini index (P.gp)
    gp <- 2 * (1 + (((mean / povline) - 1) * pov_gap / pov_gap_sq))

    watt <- gd_compute_watts_lq(headcount, mean, povline, 0.01, A, B, C)
  }

  return(
    list(
      headcount = headcount,
      pg = pov_gap,
      p2 = pov_gap_sq,
      eh = eh,
      epg = epg,
      ep = ep,
      gh = gh,
      gpg = gpg,
      gp = gp,
      watt = watt,
      dl = dl,
      ddl = ddl
    )
  )
}

#' Estimates poverty and inequality stats from Quadratic Lorenz fit
#'
#' @param n_obs numeric: number of observations
#' @param mean numeric: Welfare mean
#' @param povline numeric: Poverty line
#' @param p0 numeric: TO document
#' @param A numeric vector: Lorenz curve coefficient. Output of regres_lq()$coef[1]
#' @param B numeric vector: Lorenz curve coefficient. Output of regres_lq()$coef[2]
#' @param C numeric vector: Lorenz curve coefficient. Output of regres_lq()$coef[3]
#'
#' @return list
#' @export
#'
#' @examples
#' estimate_lq(n_obs, mean, povline, p0, coefs)
#'
gd_estimate_lq <- function(n_obs, mean, povline, p0, A, B, C) {

  # Compute key numbers from Lorenz quadratic form
  # Theorem 3 from original lorenz quadratic paper
  e <- -(A + B + C + 1) # e = -(A + B + C + 1): condition for the curve to go through (1, 1)
  m <- (B^2) - (4 * A) # m < 0: condition for the curve to be an ellipse (m is called alpha in paper)
  n <- (2 * B * e) - (4 * C) # n is called Beta in paper
  r <- (n^2) - (4 * m * e^2) # r is called K in paper

  validity <- check_curve_validity_lq(A, B, C, e, m, n, r)

  r <- sqrt(r)
  s1 <- (r - n) / (2 * m)
  s2 <- -(r + n) / (2 * m)

  # Compute distributional measures -----------------------------------------

  dist_stats <- gd_compute_dist_stats_lq(mean, p0, A, B, C, e, m, n, r)


  # Compute poverty stats ---------------------------------------------------

  pov_stats <- gd_compute_poverty_stats_lq(mean, povline, A, B, C, e, m, n, r, s1, s2)

  out <- list(gini = dist_stats$gini,
              median = dist_stats$median,
              rmhalf = dist_stats$rmhalf,
              pol = dist_stats$polarization,
              ris = dist_stats$ris,
              mld = dist_stats$mld,
              dcm = dist_stats$dcm,
              P.Decile = dist_stats$deciles,
              headcount = pov_stats$headcount,
              pg = pov_stats$pg,
              p2 = pov_stats$p2,
              eh = pov_stats$eh,
              epg = pov_stats$epg,
              ep = pov_stats$ep,
              gh = pov_stats$gh,
              gpg = pov_stats$gpg,
              gp = pov_stats$gp,
              watt = pov_stats$watt,
              dl = pov_stats$dl,
              ddl = pov_stats$ddl,
              is_normal = validity$is_normal,
              is_valid = validity$is_valid)

  return(out)

}

#' Computes the sum of squares of error
#' Measures the fit of the model to the data.
#'
#' @param welfare numeric: Welfare vector (grouped)
#' @param population numeric: Population vector (grouped)
#' @param headcount numeric: headcount index
#' @param A numeric vector: Lorenz curve coefficient
#' @param B numeric vector: Lorenz curve coefficient
#' @param C numeric vector: Lorenz curve coefficient
#'
#' @return list
#'
gd_compute_fit_lq <- function(welfare,
                              population,
                              headcount,
                              A,
                              B,
                              C) {
  lasti  <- -1
  sse  <- 0 # Sum of square error
  ssez <- 0

  for (i in seq_along(welfare[-1])) {
    residual <- welfare[i] - value_at_lq(population[i], A, B, C)
    residual_sq <- residual^2
    sse <- sse + residual_sq
    if (population[i] < headcount)
    {
      ssez <- ssez  + residual_sq
      lasti <- i
    }
  }
  lasti <- lasti + 1
  residual <- welfare[lasti] - value_at_lq(population[lasti], A, B, C)
  ssez <- ssez + residual^2

  out <- list(sse, ssez)
  names(out) <- list("sse", "ssez")

  return(out)
}



check_input_gd_compute_pip_stats_lq <- function(population,
                                                welfare,
                                                mean,
                                                default_ppp,
                                                type,
                                                povline,
                                                ppp,
                                                popshare,
                                                is_lq,
                                                p0){

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #---------   TYPE 1   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (type == 1) {

    #--------- check vector cumsum up to one ---------
    share_pop <- c(population[1], diff(population))
    share_wel <- c(welfare[1], diff(welfare))

    assertthat::assert_that(sum(share_pop) == 1,
                            msg = "Share of `population` does not sum up to 1")

    assertthat::assert_that(sum(share_wel) == 1,
                            msg = "Share of `welfare` does not sum up to 1")

    #--------- make sure  share of income is always increasing ---------
    # normalize welfare by population
    norm_wel <- diff(share_wel/share_pop)

    assertthat::assert_that(all(norm_wel >= 0),
                            msg = paste0("share of `welfare` must increase with each\n",
                                    "subsequent bin relative to its corresponging\n",
                                    "population. Make sure data is sorted correctly."))

  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #---------   TYPE 2   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (type == 2) {

    #---------  make sure data is sorted ---------
    o          <- order(welfare)

    welfare    <- welfare[o]
    population <- population[o]
  }



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #---------   TYPE 5/3   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





}