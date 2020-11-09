#' Computes poverty statistics from grouped data
#'
#' @param population numeric: cumulative proportion of population
#' @param welfare numeric: cumulative proportion of income held by that
#' proportion of the population (Lorenz Curve).
#' @param mean numeric: Welfare mean
#' @param povline numeric: Poverty line
#' @param popshare numeric: Share of population living below the poverty line.
#' Optional
#' @param default_ppp numeric: Default purchasing power parity
#' @param ppp numeric: PPP request by user
#' @param p0 numeric: To document
#'
#' @return list
#'
#' @export
#'
#' @examples
#' L <- c(0.00208, 0.01013, 0.03122, 0.07083, 0.12808, 0.23498, 0.34887,
#' 0.51994, 0.6427, 0.79201, 0.86966, 0.91277, 1)
#' P <- c(0.0092, 0.0339, 0.085, 0.164, 0.2609, 0.4133, 0.5497, 0.7196,
#' 0.8196, 0.9174, 0.957, 0.9751, 1)
#' mu  <- 109.9 # mean
#' z   <- 89    # poverty line
#' gd_compute_pip_stats_lq(P, L, mu, z)
#'
#' res <- gd_compute_pip_stats_lq(P, L, mu, z)
#' res$headcount
#' res2 <- gd_compute_pip_stats_lq(P, L, mu, popshare = res$headcount)
#' res2$povline
#
gd_compute_pip_stats_lq <- function(population,
                                    welfare,
                                    mean,
                                    povline = NULL,
                                    popshare = NULL,
                                    default_ppp = NULL,
                                    ppp = NULL,
                                    p0 = 0.5) {
  # Adjust mean if different PPP value is provided
  if (!is.null(ppp)) {
    mean <- mean * default_ppp / ppp
  } else {
      ppp <- default_ppp
    }
  # STEP 1: Prep data to fit functional form
  prepped_data <- create_functional_form_lb(population, welfare)

  # STEP 2: Estimate regression coefficients using LB parameterization
  reg_results <- regres(prepped_data)
  reg_coef <- reg_results$coef

  A <- exp(reg_coef[1]) # Why do we use exp() here?
  B <- reg_coef[2]
  C <- reg_coef[3]

  # OPTIONAL: Only when popshare is supplied
  # return poverty line if share of population living in poverty is supplied
  # intead of a poverty line
  if (!is.null(popshare)) {
    povline <- derive_lb(popshare, A, B, C) * mean
  }

  # Boundary conditions (Why 4?)
  z_min <- mean * derive_lb(0.001, A, B, C) + 4
  z_max <- mean * derive_lb(0.980, A, B, C) - 4
  z_min <- ifelse(z_min < 0, 0, z_min)

  results1 <- list(mean, povline, z_min, z_max, ppp)
  names(results1) <- list("mean", "povline", "z_min", "z_max", "ppp")

  # STEP 3: Estimate poverty measures based on identified parameters
  results2 <- gd_estimate_lq(mean, povline, p0, A, B, C)

  # STEP 4: Compute measure of regression fit
  results_fit <- gd_compute_fit_lb(welfare, population, results2$headcount, A, B, C)

  res <- c(results1, results2, results_fit, reg_results)

  return(res)

}

#' Prepares data for lorenz beta regression
#'
#' @description  Prepares data for regression. The last observation of (p,l), which by construction has the value
#' (1, 1), is excluded since the functional form for the Lorenz curve already
#' forces it to pass through the point (1, 1).
#'
#' @param welfare numeric: Welfare vector from empirical Lorenz curve
#' @param population numeric: Population vector from empirical Lorenz curve
#'
#' @return data.frame
#'
#' @seealso \href{https://econpapers.repec.org/article/ecmemetrp/v_3a48_3ay_3a1980_3ai_3a2_3ap_3a437-46.htm}{Original Beta Lorenz curve paper}

create_functional_form_lq <- function(welfare,
                                      population) {
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
  y <-  log(lorenz_pop - lorenz_welfare)
  # x1
  x1 <- 1
  # x2
  x2 <- log(lorenz_pop)
  # x3
  x3 <- log(1 - lorenz_pop)

  out <- data.frame(y, x1, x2, x3, stringsAsFactors = FALSE)

  return(out)
}


#' Returns the first derivative of the beta Lorenz
#'
#' `derive_lb()` returns the first derivative of the beta Lorenz curves
#'
#' @param x numeric: point on curve
#' @param A numeric vector: lorenz curve coefficient
#' @param B numeric vector: lorenz curve coefficient
#' @param C numeric vector: lorenz curve coefficient
#'
#' @return numeric
#'

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

#' Check validity of lorenz beta fit
#'
#' `check_curve_validity_lb()` checks the validity of the lorenz beta fit
#'
#' @param A numeric: First regression coefficient
#' @param B numeric: Second regression coefficient
#' @param C numeric: Third regression coefficient
#'
#' @return list
#'
#' @seealso \href{https://econpapers.repec.org/article/ecmemetrp/v_3a48_3ay_3a1980_3ai_3a2_3ap_3a437-46.htm}{Original Beta Lorenz curve paper}
#' @seealso \href{https://www.ifpri.org/cdmref/p15738coll2/id/125673}{
#' Computational Tools For Poverty Measurement And Analysis}
#'
check_curve_validity_lb <- function(A, B, C) {

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

  return(list(is_valid = is_valid))

}

#' Compute gini index from lorenz beta fit
#'
#' `gd_compute_gini_lb()` computes the gini index from a lorenz beta fit
#'
#' @param A numeric: First regression coefficient
#' @param B numeric: Second regression coefficient
#' @param C numeric: Third regression coefficient
#' @param nbins numeric: Number of bins used to compute gini
#'
#' @return numeric
#'
#' @seealso \href{https://www.ifpri.org/cdmref/p15738coll2/id/125673}{
#' Computational Tools For Poverty Measurement And Analysis}
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
#' `value_at_lb()`solves for beta Lorenz curves
#'
#' @param x numeric: point on curve
#' @param A numeric vector: First lorenz curve coefficient
#' @param B numeric vector: Second lorenz curve coefficient
#' @param C numeric vector: Third lorenz curve coefficient
#'
#' @return numeric
#'
value_at_lb <- function(x, A, B, C) {

  out <- x - (A * (x^B) * ((1 - x)^C))

  return(out)
}

#' Computes MLD from lorenz beta fit
#'
#' `gd_compute_mld_lb()` computes the Mean Log deviation (MLD) from a Lorenz
#' beta fit
#'
#' @param dd numeric
#' @param A numeric vector: lorenz curve coefficient
#' @param B numeric vector: lorenz curve coefficient
#' @param C numeric vector: lorenz curve coefficient
#'
#' @return numeric
#'
gd_compute_mld_lb <- function(dd, A, B, C) {
  x1 <- derive_lb(0.0005, A, B, C)
  gap <- 0
  mld <- 0
  if (x1 == 0) {
    gap <- 0.0005
  }
  else {
    mld <- log(x1) * 0.001
  }
  x1 <- derive_lb(0, A, B, C)
  for (xstep in seq(0, 0.998, 0.001)) {
    x2 <- derive_lb(xstep + 0.001, A, B, C)
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
#' `gd_compute_quantile_lb()` computes quantiles from a lorenz beta fit.
#'
#' @param A numeric vector: lorenz curve coefficient
#' @param B numeric vector: lorenz curve coefficient
#' @param C numeric vector: lorenz curve coefficient
#' @param n_quantile numeric: Number of quantiles to return
#'
#' @return numeric
#'
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
#'
gd_compute_watts_lb <- function(headcount, mu, povline, dd, A, B, C) {
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

#' Computes polarization index from beta Lorenz fit
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
gd_compute_polarization_lb <- function(mean,
                                       p0,
                                       dcm,
                                       A, B, C) {

  pol <- 2 - (1 / p0) +
    (dcm - (2 * value_at_lb(p0, A, B, C) * mean)) /
    (p0 * mean * derive_lb(p0, A, B, C))

  return(pol)
}

#' Computes distributional stats from lorenz beta fit
#'
#' @param mean numeric: welfare mean
#' @param p0 numeric: To document
#' @param A numeric: First regression coefficient
#' @param B numeric: Second regression coefficient
#' @param C numeric: Third regression coefficient
#'
#' @return list
#'
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

#' Computes poverty stats from lorenz beta fit
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
#' @param s2 numeric: To document
#'
#' @return list
#'
gd_compute_poverty_stats_lb <- function(mean,
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
    pov_gap <- headcount - (u * value_at_lb(headcount, A, B, C))

    # P.p2 - Distributionally sensitive FGT poverty measure
    #P.p2 <- (2*P.pg) - P.h - u^2 * (A*P.h + B*value_at_lb(P.h, A, B, C) - (r/16 *log((1 - P.h/s1))/(1 - P.h/s2)))
    # Poverty severity
    pov_gap_sq <- (2 * pov_gap) - headcount -
      (u^2 * (A * headcount + B * value_at_lb(headcount, A, B, C) -
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

    watt <- gd_compute_watts_lb(headcount, mean, povline, 0.01, A, B, C)
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

#' Estimates poverty and inequality stats from beta Lorenz fit
#'
#' @param mean numeric: Welfare mean
#' @param povline numeric: Poverty line
#' @param p0 numeric: TO document
#' @param A numeric vector: Lorenz curve coefficient. Output of `regres_lq()$coef[1]`
#' @param B numeric vector: Lorenz curve coefficient. Output of `regres_lq()$coef[2]`
#' @param C numeric vector: Lorenz curve coefficient. Output of `regres_lq()$coef[3]`
#'
#' @return list
#' @export
#'
#'
gd_estimate_lb <- function(mean, povline, p0, A, B, C) {

  validity <- check_curve_validity_lb(A, B, C)

  # Compute distributional measures -----------------------------------------

  dist_stats <- gd_compute_dist_stats_lb(mean, p0, A, B, C)


  # Compute poverty stats ---------------------------------------------------

  pov_stats <- gd_compute_poverty_stats_lb(mean, povline, A, B, C, e, m, n, r, s1, s2)

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
gd_compute_fit_lb <- function(welfare,
                              population,
                              headcount,
                              A,
                              B,
                              C) {
  lasti  <- -1
  sse  <- 0 # Sum of square error
  ssez <- 0

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

  return(out)
}

#' DDLK
#'
#' @param h numeric
#' @param A numeric vector: lorenz curve coefficient
#' @param B numeric vector: lorenz curve coefficient
#' @param C numeric vector: lorenz curve coefficient
#'
#' @return numeric
#' @export
#'
#' @examples
#' DDLK(h, A, B, C)
#'
DDLK <- function(h, A, B, C) {
  tmp1 <- B * (1 - B) / (h^2)
  tmp2 <- (2 * B * C) / (h * (1 - h))
  tmp3 <- C * (1 - C) / ((1 - h)^2)
  res <- A * (h^B) * ((1 - h)^C) * (tmp1 + tmp2 + tmp3)
  return(res)
}

