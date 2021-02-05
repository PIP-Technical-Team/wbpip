#' Computes poverty statistics (Lorenz beta)
#'
#' Compute poverty statistics for grouped data using the beta functional form of
#' the Lorenz qurve.
#'
#' @inheritParams gd_compute_pip_stats
#' @return list
#' @keywords internal
gd_compute_pip_stats_lb <- function(welfare,
                                    povline,
                                    population,
                                    requested_mean,
                                    popshare = NULL,
                                    default_ppp = NULL,
                                    ppp = NULL,
                                    p0 = 0.5) {

  # Adjust mean if different PPP value is provided
  if (!is.null(ppp)) {
    requested_mean <- requested_mean * default_ppp / ppp
  } else {
    ppp <- default_ppp
  }
  # STEP 1: Prep data to fit functional form
  prepped_data <- create_functional_form_lb(welfare = welfare,
                                            population = population)

  # STEP 2: Estimate regression coefficients using LB parameterization
  reg_results <- regres(prepped_data, is_lq = FALSE)
  reg_coef <- reg_results$coef

  A <- reg_coef[1]
  B <- reg_coef[2]
  C <- reg_coef[3]

  # OPTIONAL: Only when popshare is supplied
  # return poverty line if share of population living in poverty is supplied
  # intead of a poverty line

  if (!is.null(popshare)) {
    povline <- derive_lb(popshare, A, B, C) * requested_mean
  }

  # Boundary conditions (Why 4?)
  z_min <- requested_mean * derive_lb(0.001, A, B, C) + 4
  z_max <- requested_mean * derive_lb(0.980, A, B, C) - 4
  z_min <- ifelse(z_min < 0, 0, z_min)

  results1 <- list(requested_mean, povline, z_min, z_max, ppp)
  names(results1) <- list("mean", "poverty_line", "z_min", "z_max", "ppp")

  # STEP 3: Estimate poverty measures based on identified parameters
  results2 <- gd_estimate_lb(requested_mean, povline, p0, A, B, C)

  # STEP 4: Compute measure of regression fit
  results_fit <- gd_compute_fit_lb(welfare, population, results2$headcount, A, B, C)

  res <- c(results1, results2, results_fit, reg_results)

  return(res)

}

#' create_functional_form_lb
#'
#' Prepare data for Lorenz beta regression.
#'
#' Prepares data for regression. The last observation of (p,l), which by
#' construction has the value (1, 1), is excluded since the functional form for
#' the Lorenz curve already forces it to pass through the point (1, 1).
#'
#' @param welfare numeric: Welfare vector from empirical Lorenz curve
#' @param population numeric: Population vector from empirical Lorenz curve
#'
#' @references
#' Kakwani, N. 1980. "[On a Class of Poverty
#' Measures](https://EconPapers.repec.org/RePEc:ecm:emetrp:v:48:y:1980:i:2:p:437-46)".
#' *Econometrica 48* (2): 437-46.
#'
#' @return data.frame
#' @keywords internal
create_functional_form_lb <- function(welfare, population) {
  # CHECK inputs
  assertthat::assert_that(is.numeric(population))
  assertthat::assert_that(is.numeric(welfare))
  assertthat::assert_that(length(population) == length(welfare))
  assertthat::assert_that(length(population) > 1)

  # Remove last observation (the functional form for the Lorenz curve already forces
  # it to pass through the point (1, 1)
  nobs <- length(population) - 1
  population <- population[1:nobs]
  welfare <- welfare[1:nobs]

  # y
  y <-  log(population - welfare)
  # x1
  x1 <- 1
  # x2
  x2 <- log(population)
  # x3
  x3 <- log(1 - population)

  out <- data.frame(y, x1, x2, x3, stringsAsFactors = FALSE)

  return(out)
}

#' Returns the first derivative of the beta Lorenz
#'
#' `derive_lb()` returns the first derivative of a beta Lorenz curve.
#'
#' @param x numeric: Point on curve.
#' @param A numeric: Lorenz curve coefficient.
#' @param B numeric: Lorenz curve coefficient.
#' @param C numeric: Lorenz curve coefficient.
#'
#' @return numeric
#' @keywords internal
derive_lb <- function(x, A, B, C) {
  if (x == 0) {
    if (B == 1) {return(1 - A)}
    if (B > 1) {return(1)}
    return(-Inf)
  } else if (x == 0) {
    if (C == 1) {return(1 + A)}
    if (C > 1) {return(1)}
    return(Inf)
  }

  # Formula for first derivative of GQ Lorenz Curve
  val <- 1 - (A * x^B * (1 - x)^C * ((B / x) - C / (1 - x)))

  return(val)
}

#' Check validity of Lorenz beta fit
#'
#' `check_curve_validity_lb()` checks the validity of the Lorenz beta fit
#'
#' @param headcount numeric: Poverty rate computed by
#'   gd_compute_poverty_stats_lb().
#' @param A numeric: First regression coefficient.
#' @param B numeric: Second regression coefficient.
#' @param C numeric: Third regression coefficient.
#'
#' @references
#' Datt, G. 1998. "[Computational Tools For Poverty Measurement And
#' Analysis](https://www.ifpri.org/cdmref/p15738coll2/id/125673)". FCND
#' Discussion Paper 50. World Bank, Washington, DC.
#'
#' Kakwani, N. 1980. "[On a Class of Poverty
#' Measures](https://EconPapers.repec.org/RePEc:ecm:emetrp:v:48:y:1980:i:2:p:437-46)".
#' *Econometrica 48* (2): 437-46.
#'
#' @return list
#' @keywords internal
check_curve_validity_lb <- function(headcount, A, B, C) {

  is_valid <- TRUE

  for (w in seq(from = 0.001, to = 0.1, by = 0.05)) {
    if (derive_lb(w, A, B, C) < 0) {
      is_valid <- FALSE
      break
    }
  }

  if (is_valid) {
    for (w in seq(from = 0.001, to = 0.999, by = 0.05)) {
      if (DDLK(w, A, B, C) < 0) { # What does DDLK stands for?? What does it do?
        is_valid <- FALSE
        break
      }
    }
  }

  # WHAT IS THE RATIONAL HERE?
  is_normal <- if (!is.na(headcount)) {
    is_normal <- TRUE
  } else {
    is_normal <- FALSE
  }

  return(list(
    is_valid = is_valid,
    is_normal = is_normal
  )
  )

}

#' Compute gini index from Lorenz beta fit
#'
#' `gd_compute_gini_lb()` computes the gini index from a Lorenz beta fit.
#'
#' @param A numeric: First regression coefficient.
#' @param B numeric: Second regression coefficient.
#' @param C numeric: Third regression coefficient.
#' @param nbins numeric: Number of bins used to compute Gini.
#'
#' @references Datt, G. 1998. "[Computational Tools For Poverty Measurement And
#' Analysis](https://www.ifpri.org/cdmref/p15738coll2/id/125673)". FCND
#' Discussion Paper 50. World Bank, Washington, DC.
#'
#' @return numeric
#' @keywords internal
gd_compute_gini_lb <- function(A, B, C,nbins = 499) {

  out <- vector(mode = "numeric", length = nbins)

  for (i in seq(from = 0, to = nbins, by = 1)) {
    x <- (i * 0.002) + 0.001
    out[i] <- 4 * value_at_lb(x, A, B, C) + 2 * value_at_lb(x + 0.001, A, B, C)
  }

  gini <- sum(out)
  gini <- 1 - ((gini - 1) / 1500) # Why 1500? Why is it hardcoded?
  return(gini)

}

#' Solves for beta Lorenz curves
#'
#' `value_at_lb()`solves for beta Lorenz curves.
#'
#' @param x numeric: Point on curve.
#' @param A numeric: First Lorenz curve coefficient.
#' @param B numeric: Second Lorenz curve coefficient.
#' @param C numeric: Third Lorenz curve coefficient.
#'
#' @return numeric
#' @keywords internal
value_at_lb <- function(x, A, B, C) {

  out <- x - (A * (x^B) * ((1 - x)^C))

  return(out)
}

#' Computes MLD from Lorenz beta fit
#'
#' `gd_compute_mld_lb()` computes the Mean Log deviation (MLD) from a Lorenz
#' beta fit.
#'
#' @param dd numeric: **TO BE DOCUMENTED**.
#' @param A numeric: Lorenz curve coefficient.
#' @param B numeric: Lorenz curve coefficient.
#' @param C numeric: Lorenz curve coefficient.
#'
#' @return numeric
#' @keywords internal
gd_compute_mld_lb <- function(dd, A, B, C) {
  x1 <- derive_lb(0.0005, A, B, C)
  gap <- 0
  mld <- 0
  if (x1 == 0) {
    gap <- 0.0005
  }
  else {
    mld <- suppressWarnings(log(x1) * 0.001)
  }
  x1 <- derive_lb(0, A, B, C)
  for (xstep in seq(0, 0.998, 0.001)) {
    x2 <- derive_lb(xstep + 0.001, A, B, C)
    if ((x1 <= 0) || (x2 <= 0)) {
      gap <- gap + 0.001
      if (gap > 0.5) {
        return(NA)
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
#' `gd_compute_quantile_lb()` computes quantiles from a Lorenz beta fit.
#'
#' @param A numeric: Lorenz curve coefficient.
#' @param B numeric: Lorenz curve coefficient.
#' @param C numeric: Lorenz curve coefficient.
#' @param n_quantile numeric: Number of quantiles to return.
#'
#' @return numeric
#' @keywords internal
gd_compute_quantile_lb <- function(A, B, C, n_quantile = 10) {
  vec <- vector(mode = "numeric", length = n_quantile)
  x1 <- 1 / n_quantile
  q <- 0
  lastq <- 0
  for (i in seq_len(n_quantile - 1)) {
    q <- value_at_lb(x1, A, B, C)
    v <- q - lastq
    vec[i] <- v
    lastq <- q
    x1 <- x1 + 1 / n_quantile
  }
  vec[n_quantile] <- 1 - lastq

  return(vec)
}

#'  Computes Watts Index from beta Lorenz fit
#'
#' `gd_compute_watts_lb()` computes Watts Index from beta Lorenz fit
#' The first distribution-sensitive poverty measure was proposed in 1968 by Watts
#' It is defined as the mean across the population of the proportionate poverty
#' gaps, as measured by the log of the ratio of the poverty line to income,
#' where the mean is formed over the whole population, counting the nonpoor as
#' having a zero poverty gap.
#'
#' @param headcount numeric: Headcount index.
#' @param mean numeric: Welfare mean.
#' @param povline numeric: Poverty line.
#' @param dd numeric: **TO BE DOCUMENTED**.
#' @param A numeric: Lorenz curve coefficient.
#' @param B numeric: Lorenz curve coefficient.
#' @param C numeric: Lorenz curve coefficient.
#'
#' @return numeric
#' @keywords internal
gd_compute_watts_lb <- function(headcount, mean, povline, dd, A, B, C) {
  if (headcount <= 0 | is.na(headcount)) {
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

  x1 <- derive_lb(snw / 2, A, B, C)
  if (x1 <= 0) {
    gap <- snw / 2
  } else {
    watts <- log(x1) * snw
  }
  xend <- headcount - snw
  x1 <- derive_lb(0, A, B, C)
  # Number of steps seems to be different from what happens in .Net codebase
  for (xstep in seq(0, xend, by = snw)) {
    x2 <- derive_lb(xstep + snw, A, B, C)
    if ((x1 <= 0) || (x2 <= 0)) {
      gap <- gap + snw
      if (gap > 0.05) {
        return(-1)
      }
    } else {
      gap <- 0
      watts <- watts + (log(x1) + log(x2)) * snw * 0.5
    }
    x1 <- x2
  }
  if ((mean != 0) && (watts != 0)) {
    x1 <- povline / mean
    if (x1 > 0) {
      watts <- log(x1) * headcount - watts
      if (watts > 0) {
        return(watts)
      }
    }
    return(-1) # Negative Watts values will be handled in gd_select_lorenz()
  }
}

#' Computes distributional stats from Lorenz beta fit
#'
#' @param mean numeric: Welfare mean.
#' @param p0 numeric: **TO BE DOCUMENTED**.
#' @param A numeric: First regression coefficient.
#' @param B numeric: Second regression coefficient.
#' @param C numeric: Third regression coefficient.
#'
#' @return list
#' @keywords internal
gd_compute_dist_stats_lb <- function(mean, p0, A, B, C) {

  gini    <- gd_compute_gini_lb(A, B, C)
  median  <- mean * derive_lb(0.5, A, B, C)
  rmhalf  <- value_at_lb(p0, A, B, C) * mean / p0 # What is this??
  dcm     <- (1 - gini) * mean
  pol     <- gd_compute_polarization_lb(mean, p0, dcm, A, B, C)
  ris     <- value_at_lb(0.5, A, B, C)
  mld     <- gd_compute_mld_lb(0.01, A, B, C)
  deciles <- gd_compute_quantile_lb(A, B, C)

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

#' Computes polarization index from parametric Lorenz fit
#'
#' Used for grouped data computations,
#'
#' @param mean numeric: Welfare mean.
#' @param p0 numeric: **TO BE DOCUMENTED**
#' @param dcm numeric: **TO BE DOCUMENTED**
#' @param A numeric: Lorenz curve coefficient.
#' @param B numeric: Lorenz curve coefficient.
#' @param C numeric: Lorenz curve coefficient.
#'
#' @return numeric
#' @keywords internal
gd_compute_polarization_lb <- function(mean,
                                       p0,
                                       dcm,
                                       A, B, C) {

  pol <- 2 - (1 / p0) +
    (dcm - (2 * value_at_lb(p0, A, B, C) * mean)) /
    (p0 * mean * derive_lb(p0, A, B, C))

  return(pol)
}

#' Computes poverty stats from Lorenz beta fit
#'
#' @param mean numeric: Welfare mean.
#' @param povline numeric: Poverty line.
#' @param A numeric: First regression coefficient.
#' @param B numeric: Second regression coefficient.
#' @param C numeric: Third regression coefficient.
#'
#' @return list
#' @keywords internal
gd_compute_poverty_stats_lb <- function(mean,
                                        povline,
                                        A,
                                        B,
                                        C) {
  # Compute headcount
  headcount <- gd_compute_headcount_lb(mean = mean,
                                       povline = povline,
                                       A = A,
                                       B = B,
                                       C = C)

  # Poverty gap
  u <- mean / povline
  pov_gap <- gd_compute_pov_gap_lb(u, headcount, A, B, C)

  # Poverty severity
  pov_gap_sq <- gd_compute_pov_severity_lb(u, headcount, pov_gap, A, B, C)

  # First derivative of the Lorenz curve
  dl <- 1 - A * (headcount^B) * ((1 - headcount)^C) * (B / headcount - C / (1 - headcount))

  # Second derivative of the Lorenz curve
  ddl <- A * (headcount^B) *
    ((1 - headcount)^C) *
    ((B * (1 - B) / headcount^2) +
       (2 * B * C / (headcount * (1 - headcount))) +
       (C * (1 - C) / ((1 - headcount)^2)))

  # Elasticity of headcount index w.r.t mean
  eh <- -povline / (mean * headcount * ddl)

  # Elasticity of poverty gap index w.r.t mean
  epg <- 1 - (headcount / pov_gap)

  # Elasticity of distributionally sensitive FGT poverty measure w.r.t mean
  ep <- 2 * (1 - pov_gap / pov_gap_sq)

  # PElasticity of headcount index w.r.t gini index
  gh <- (1 - povline / mean) / (headcount  * ddl)

  # Elasticity of poverty gap index w.r.t gini index
  gpg <- 1 + (((mean / povline) - 1) * headcount / pov_gap)

  # Elasticity of distributionally sensitive FGT poverty measure w.r.t gini index
  gp <- 2 * (1 + (((mean / povline) - 1) * pov_gap / pov_gap_sq))

  # Watts index
  watts <- gd_compute_watts_lb(headcount, mean, povline, 0.005, A, B, C)

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
      watts = watts,
      dl = dl,
      ddl = ddl
    )
  )
}

#' Estimates poverty and inequality stats from beta Lorenz fit
#'
#' @param mean numeric: Welfare mean.
#' @param povline numeric: Poverty line.
#' @param p0 numeric: **TO BE DOCUMENTED**.
#' @param A numeric: Lorenz curve coefficient. Output of
#'   `regres()$coef[1]`.
#' @param B numeric: Lorenz curve coefficient. Output of
#'   `regres()$coef[2]`.
#' @param C numeric: Lorenz curve coefficient. Output of
#'   `regres()$coef[3]`.
#'
#' @return list
#' @keywords internal
gd_estimate_lb <- function(mean, povline, p0, A, B, C) {

  # Compute distributional measures
  dist_stats <- gd_compute_dist_stats_lb(mean, p0, A, B, C)

  # Compute poverty stats
  pov_stats <- gd_compute_poverty_stats_lb(mean, povline, A, B, C)

  # Check validity
  validity <- check_curve_validity_lb(headcount = pov_stats[["headcount"]], A, B, C)

  out <- list(gini = dist_stats$gini,
              median = dist_stats$median,
              rmhalf = dist_stats$rmhalf,
              polarization = dist_stats$polarization,
              ris = dist_stats$ris,
              mld = dist_stats$mld,
              dcm = dist_stats$dcm,
              deciles = dist_stats$deciles,
              headcount = pov_stats$headcount,
              poverty_gap = pov_stats$pg,
              poverty_severity = pov_stats$p2,
              eh = pov_stats$eh,
              epg = pov_stats$epg,
              ep = pov_stats$ep,
              gh = pov_stats$gh,
              gpg = pov_stats$gpg,
              gp = pov_stats$gp,
              watts = pov_stats$watts,
              dl = pov_stats$dl,
              ddl = pov_stats$ddl,
              is_normal = validity$is_normal,
              is_valid = validity$is_valid)

  return(out)

}

#' Computes the sum of squares of error
#' Measures the fit of the model to the data.
#'
#' @param welfare numeric: Welfare vector (grouped).
#' @param population numeric: Population vector (grouped).
#' @param headcount numeric: Headcount index.
#' @param A numeric: Lorenz curve coefficient.
#' @param B numeric: Lorenz curve coefficient.
#' @param C numeric: Lorenz curve coefficient.
#'
#' @return list
#' @keywords internal
gd_compute_fit_lb <- function(welfare,
                              population,
                              headcount,
                              A,
                              B,
                              C) {

  if (!is.na(headcount)) {

    lasti  <- 0
    sse  <- 0 # Sum of square error
    ssez <- 0 # Sum of square error up to poverty line threshold (see Datt paper)

    for (i in seq_along(welfare[-1])) {
      residual <- welfare[i] - value_at_lb(population[i], A, B, C)
      residual_sq <- residual^2
      sse <- sse + residual_sq
      if (population[i] < headcount)
      {
        ssez <- ssez  + residual_sq
        lasti <- i
      }
    }
    lasti <- lasti + 1
    residual <- welfare[lasti] - value_at_lb(population[lasti], A, B, C)
    ssez <- ssez + residual^2

    out <- list(sse, ssez)
    names(out) <- list("sse", "ssez")

  } else {

    out <- list(sse = NA, ssez = NA)

  }

  return(out)
}

#' DDLK
#'
#' **TO BE DOCUMENTED**
#'
#' @param h numeric **TO BE DOCUMENTED**.
#' @param A numeric: Lorenz curve coefficient.
#' @param B numeric: Lorenz curve coefficient.
#' @param C numeric: Lorenz curve coefficient.
#'
#' @return numeric
#' @noRd
DDLK <- function(h, A, B, C) {
  tmp1 <- B * (1 - B) / (h^2)
  tmp2 <- (2 * B * C) / (h * (1 - h))
  tmp3 <- C * (1 - C) / ((1 - h)^2)
  res <- A * (h^B) * ((1 - h)^C) * (tmp1 + tmp2 + tmp3)
  return(res)
}

#' Compute the headcount statistic from Lorenz Beta fit
#'
#' @param mean numeric: Welfare measure mean (income of consumption).
#' @param povline numeric: Poverty line.
#' @param A numeric: Lorenz curve coefficient.
#' @param B numeric: Lorenz curve coefficient.
#' @param C numeric: Lorenz curve coefficient.
#'
#' @return numeric
#' @keywords internal
gd_compute_headcount_lb <- function(mean, povline, A, B, C) {
  # Compute headcount
  headcount <- rtSafe(0.0001, 0.9999, 1e-4,
                      mean = mean,
                      povline = povline,
                      A = A,
                      B = B,
                      C = C)
  # Check headcount invalidity conditions
  if (headcount < 0 | is.na(headcount)) {return(NA)}

  condition1 <- is.na(BETAI(a = 2 * B - 1,
                            b = 2 * C + 1,
                            x = headcount))
  condition2 <- is.na(BETAI(a = 2 * B,
                            b = 2 * C,
                            x = headcount))
  condition3 <- is.na(BETAI(a = 2 * B + 1,
                            b = 2 * C - 1,
                            x = headcount))

  if (condition1 | condition2 | condition3) {return(NA)}

  return(headcount)
}

#' BETAI
#'
#' **TO BE DOCUMENTED**
#'
#' @param a numeric **TO BE DOCUMENTED**
#' @param b numeric **TO BE DOCUMENTED**
#' @param x numeric **TO BE DOCUMENTED**
#'
#' @return numeric
#' @noRd
BETAI <- function(a, b, x) {

  if (!is.na(x)) {
    bt <- betai <- 0

    if (x == 0 || x == 1) {
      bt <- 0
    } else {
      bt <- exp((a * log(x)) + (b * log(1 - x)))
    }

    if (x < (a + 1)/(a + b + 2))
      betai <- bt * BETAICF(a, b, x) / a
    else if (is.na(GAMMLN(a)) || is.na(GAMMLN(b)) || is.na(GAMMLN(a + b)))
      betai <- NA
    else
      betai <- exp(GAMMLN(a) + GAMMLN(b) - GAMMLN(a + b)) - (bt * BETAICF(b, a, 1 - x) / b)
  } else {
    betai <- NA
  }

  return(betai)
}

#' GAMMLN
#'
#' **TO BE DOCUMENTED**
#'
#' @param xx numeric: **TO BE DOCUMENTED**
#'
#' @return numeric
#' @noRd
GAMMLN <- function(xx) {

  cof <- list(76.18009173, -86.50532033, 24.01409822, -1.231739516, 0.120858003e-2, -0.536382e-5)
  stp <- 2.50662827465
  half <- 0.5
  one <- 1
  fpf <- 5.5
  # x = tmp = ser <- 0
  x <- 0
  tmp <- 0
  ser <- 0

  x <- xx - one
  tmp <- x + fpf
  if (tmp <= 0)
    return(NA)

  tmp <- (x + half) * log(tmp) - tmp
  ser <- one

  for (i in seq(1, 6, by = 1)) {
    x <- sum(x, one)
    ser <- sum(ser, cof[[i]] / x)
  }

  if (stp*ser <= 0)
    return(NA)

  return(tmp + log(stp * ser))
}

#' BETAICF
#'
#' **TO BE DOCUMENTED**
#'
#' @param a numeric **TO BE DOCUMENTED**
#' @param b numeric **TO BE DOCUMENTED**
#' @param x numeric **TO BE DOCUMENTED**
#'
#' @return numeric
#' @noRd
BETAICF <- function(a, b, x) {

  eps <- 3e-7
  # am = bm = az <- 1
  am <- 1
  bm <- 1
  az <- 1
  qab <- a + b
  qap <- a + 1
  qam <- a - 1
  bz <- 1 - (qab * x / qap)

  # d = app = bpp = ap = bp = aold = em = tem <- 0
  d <- 0
  app <- 0
  bpp <- 0
  ap <- 0
  bp <- 0
  aold <- 0
  em <- 0
  tem <- 0
  for (m in seq(1, 100, by = 1)) {
    em <- m
    tem <- sum(em, em)
    d <- em * (b - m) * x / ((qam + tem) * (a + tem))
    ap <- az + (d * am)
    bp <- bz + (d * bm)
    d <- -(a + em) * (qab + em) * x / ((a + tem) * (qap + tem))
    app <- ap + (d * az)
    bpp <- bp + (d * bz)
    aold <- az
    am <- ap / bpp
    bm <- bp / bpp
    az <- app / bpp
    bz <- 1
    if ((abs(az - aold)) < (eps * abs(az)))
      break
  }
  return(az)
}

#' Compute poverty gap for Lorenz Beta fit
#'
#' @param u numeric: Normalized mean.
#' @param headcount numeric: Headcount.
#' @param A numeric: First regression parameter.
#' @param B numeric: Second regression parameter.
#' @param C numeric: Third regression parameter.
#'
#' @return numeric
#' @keywords internal
gd_compute_pov_gap_lb <- function(u, headcount, A, B, C) {

  pov_gap <- headcount - (u * value_at_lb(headcount, A, B, C))
  # REVIEW RATIONAL FOR THESE ADJUSTMENTS
  # Adjust Poverty gap
  pov_gap <- ifelse(headcount < pov_gap, headcount - 0.00001, pov_gap)
  pov_gap <- ifelse(pov_gap < 0, 0, pov_gap)

  return(pov_gap)
}

#' Compute poverty severity for Lorenz Beta fit
#'
#' @param u numeric: Mean? **TO BE DOCUMENTED**.
#' @param headcount numeric: Headcount.
#' @param pov_gap numeric: Poverty gap.
#' @param A numeric: First regression parameter.
#' @param B numeric: Second regression parameter.
#' @param C numeric: Third regression parameter.
#'
#' @return numeric
#' @keywords internal
gd_compute_pov_severity_lb <- function(u, headcount, pov_gap, A, B, C) {
  u1 <- 1 - u
  beta1 <- BETAI(a = 2 * B - 1,
                 b = 2 * C + 1,
                 x = headcount)
  beta2 <- BETAI(a = 2 * B,
                 b = 2 * C,
                 x = headcount)
  beta3 <- BETAI(a = 2 * B + 1,
                 b = 2 * C - 1,
                 x = headcount)

  pov_gap_sq <- u1 * (2 * pov_gap - u1 * headcount) + A^2 * u^2 * (B^2 * beta1 - 2 * B * C * beta2 + C^2 * beta3)
  # REVIEW RATIONAL FOR THESE ADJUSTMENTS
  # Adjust Poverty severity
  pov_gap_sq <- ifelse(pov_gap < pov_gap_sq, pov_gap - 0.00001, pov_gap_sq)
  pov_gap_sq <- ifelse(pov_gap_sq < 0, 0, pov_gap_sq)

  return(pov_gap_sq)
}

#' rtSafe
#'
#' **TO BE DOCUMENTED**
#'
#' @param x1 numeric: **TO BE DOCUMENTED**
#' @param x2 numeric: **TO BE DOCUMENTED**
#' @param xacc numeric: **TO BE DOCUMENTED**
#' @param mean numeric: Welfare measure mean (income of consumption).
#' @param povline numeric: Poverty line.
#' @param A numeric: Lorenz curve coefficient.
#' @param B numeric: Lorenz curve coefficient.
#' @param C numeric: Lorenz curve coefficient.
#'
#' @return numeric
#' @noRd
rtSafe <- function(x1, x2, xacc, mean, povline, A, B, C) {

  funcCall1 <- funcD(x1, mean, povline, A, B, C)
  fl <- funcCall1[[1]]

  funcCall2 <- funcD(x2, mean, povline, A, B, C)
  fh <- funcCall2[[1]]
  df <- funcCall2[[2]]

  if (fl * fh >= 0) {
    res <- rtNewt(mean = mean, povline = povline, A = A, B = B, C = C)
    return(res)
  }

  if (fl < 0) {
    xl <- x1
    xh <- x2
  } else {
    xl <- x2
    xh <- x1
  }

  rtsafe <- 0.5 * (x1 + x2)
  dxold <- abs(x2 - x1)
  dx <- dxold

  funcCall3 <- funcD(rtsafe, mean, povline, A, B, C)
  f <- funcCall3[[1]]
  df <- funcCall3[[2]]

  temp <- 0
  for (i in seq(0, 99, by = 1)) {
    tmp <- (((rtsafe - xh) * df) - f) * (((rtsafe - xl) * df) - f)
    if (tmp >= 0 || abs(2 * f) > abs(dxold * df)) {
      dxold <- dx
      dx <- 0.5 * (xh - xl)
      rtsafe <- xl + dx
      if (xl == rtsafe) {return(rtsafe)}
    } else {
      dxold <- dx
      dx <- f/df
      temp <- temp - dx
      if (temp == rtsafe) {return(rtsafe)}
    }
    if (abs(dx) < xacc) {return(rtsafe)}

    funcCall4 <- funcD(rtsafe, mean, povline, A, B, C)
    f <- funcCall4[[1]]

    if (f < 0)
      xl <- rtsafe
    else
      xh <- rtsafe
  }

  return(NA)
}

#' funcD
#'
#' **TO BE DOCUMENTED**
#'
#' @param x numeric: **TO BE DOCUMENTED**
#' @param mean numeric: Welfare measure mean (income of consumption).
#' @param povline numeric: Poverty line.
#' @param A numeric: Lorenz curve coefficient.
#' @param B numeric: Lorenz curve coefficient.
#' @param C numeric: Lorenz curve coefficient.
#'
#' @return list
#' @noRd
funcD <- function(x, mean, povline, A, B, C) {
  x1 <- 1 - x
  v1 <- (x^B) * (x1^C)
  f <- (A * v1 * ((B/x) - (C/x1))) + (povline/mean) - 1
  df <- A * v1 * (((B/x) - (C/x1))^2 - (B/x^2) - (C/x1^2))
  return(list(f = f,
              df = df))
}

#' rtNewt
#'
#' **TO BE DOCUMENTED**
#'
#' @param mean numeric: Welfare measure mean (income of consumption).
#' @param povline numeric: Poverty line.
#' @param A numeric: Lorenz curve coefficient.
#' @param B numeric: Lorenz curve coefficient.
#' @param C numeric: Lorenz curve coefficient.
#'
#' @return numeric
#' @noRd
rtNewt <- function(mean, povline, A, B, C) {
  x1 <- 0
  x2 <- 1
  xacc <- 1e-4
  rtnewt <- 0.5 * (x1 + x2)

  for (i in seq(0, 19, by = 1)) {
    x <- rtnewt
    v1 <- (x^B) * ((1 - x)^C)
    f <- A * v1 * ((B / x) - C/(1 - x)) + (povline / mean) - 1
    df <- A * v1 * (((B / x) - C / (1 - x))^2 - (B / x^2) - (C / (1 - x)^2))
    dx <- f / df
    rtnewt <- rtnewt - dx
    if ((x1 - rtnewt) * (rtnewt - x2) < 0) {
      rtnewt <- ifelse(rtnewt < x1, 0.5 * (x2 - x), 0.5 * (x - x1))
    } else {
      if (abs(dx) < xacc)
        return(rtnewt)
    }
  }
  return(NA)
}
