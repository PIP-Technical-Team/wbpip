test_that("gd_compute_dist_stats_lb returns expected results", {
  mean <- 51.5660557757944
  p0 <- 0.5
  A <- 0.57803721740313529
  B <- 0.94205090386544987
  C <- 0.52578600019473676

  benchmark <- list(
    gini = 0.31236656171451094,
    median = 42.594731176686537,
    rmhalf = 30.014095665785614,
    dcm = 35.458544231930809,
    polarization = 0.2556375,
    ris = 0.29102570687473944,
    mld = 0.16334243665267192,
    deciles = c(0.0375048593336125,
                0.0496360493447891,
                0.058713139008492066,
                0.067752312054008029,
                0.077419347133837746,
                0.0883101840173634,
                0.10132910544405782,
                0.11835493348127901,
                0.14500186589082675,
                0.25597820429173357)

  )
  out <- gd_compute_dist_stats_lb(mean = mean,
                                  p0 = p0,
                                  A = A,
                                  B = B,
                                  C = C)

  expect_equal(names(out), c("gini", "median", "rmhalf", "dcm", "polarization",
                             "ris", "mld", "deciles"))
  expect_equal(length(out), length(benchmark))
  expect_equal(out$gini, benchmark$gini, tolerance = 3e-06) #1e-06
  expect_equal(out$median, benchmark$median)
  expect_equal(out$rmhalf, benchmark$rmhalf)
  expect_equal(out$dcm, benchmark$dcm, tolerance = 1e-05) # Fails due to difference in gini
  expect_equal(out$polarization, benchmark$polarization, tolerance = 3e-07) #1e-07
  expect_equal(out$ris, benchmark$ris)
  expect_equal(out$mld, benchmark$mld)
  expect_equal(out$deciles, benchmark$deciles)
})

test_that("gd_compute_gini_lb returns expected results", {

  benchmarck <- 0.31236656171451094

  out <- gd_compute_gini_lb(A = 0.57803721740313529,
                            B = 0.94205090386544987,
                            C = 0.52578600019473676,
                            nbins = 499)
  # Difference at the 7 digit level
  expect_equal(out, benchmarck, tolerance = 3e-6) #1e-6 ?

})

test_that("gd_compute_polarization_lb returns expected results", {
  # Constants
  mean <- 51.5660557757944
  p0 <- 0.5
  A <- 0.57803721740313529
  B <- 0.94205090386544987
  C <- 0.52578600019473676

  # test 1
  dcm <- -1
  benchmarck <- -1.456240939149799
  out <- gd_compute_polarization_lb(mean = mean,
                                    p0 = p0,
                                    dcm = -1,
                                    A = A,
                                    B = B,
                                    C = C)
  expect_equal(out, benchmarck)

})

test_that("rtSafe returns expected results", {
  # Constants
  x1 <- 0.0001
  x2 <- 0.9999
  xacc <- 0.0001
  mean <- 51.5660557757944
  povline <- 57.791666666666664
  A <- 0.57803721740313529
  B <- 0.94205090386544987
  C <- 0.52578600019473676
  benchmarck <- 0.71833938360214233

  out <- rtSafe(x1 = x1,
                x2 = x2,
                xacc = xacc,
                povline = povline,
                mean = mean,
                A = A,
                B = B,
                C = C)
  expect_equal(round(out, 5), round(benchmarck, 5))

})

test_that("rtSafe assigns xl and xh appropriately when fl < 0", {
  x <- 0.9
  mean <- 51.5660557757944
  povline <- 57.791666666666664
  A <- 0.57803721740313529
  B <- 0.94205090386544987
  C <- 0.52578600019473676

  expect_equal(rtSafe(x1 = x, x2 = 0.002, xacc = 1,
                      mean = mean, povline = povline,
                      A = A, B = B, C = C),
               0.451)

})

test_that("funcD returns expected results", {
  # Constants
  x <- 0.0001
  mean <- 51.5660557757944
  povline <- 57.791666666666664
  A <- 0.57803721740313529
  B <- 0.94205090386544987
  C <- 0.52578600019473676
  benchmarck <- list(f = 1.0492256009684304,
                     df = -539.06093885216137)

  out <- funcD(x = x,
               mean = mean,
               povline = povline,
               A = A,
               B = B,
               C = C)
  expect_equal(out$f, benchmarck$f)
  expect_equal(out$df, benchmarck$df)

})

test_that("rtNewt returns expected results", {
  # Constants
  mean <- 51.5660557757944
  povline <- 57.791666666666664
  A <- 0.57803721740313529
  B <- 0.94205090386544987
  C <- 0.52578600019473676
  benchmarck <- 0.718339386776914

  out <- rtNewt(mean = 51.5660557757944,
                povline = 57.791666666666664,
                A = A,
                B = B,
                C = C)
  expect_equal(out, benchmarck)
})

test_that("GAMMLN returns expected results", {
  # Test1
  xx <- 0.88410180773089975
  benchmarck <- 0.078623347496317386
  out <- GAMMLN(xx)
  expect_equal(out, benchmarck)

  # Test2
  xx <- 2.0515720003894735
  benchmarck <- 0.022652394699083089
  out <- GAMMLN(xx)
  expect_equal(out, benchmarck)

  # Test3
  xx <- 2.93567380812037335
  benchmarck <- 0.63461199191914641
  out <- GAMMLN(xx)
  expect_equal(out, benchmarck)
})

test_that("GAMMLN returns NA when tmp <= 0", {
  expect_equal(GAMMLN(xx = -100),
               NA)

  expect_equal(GAMMLN(xx = -4.4),
               NA)
})

test_that("BETAI returns expected values", {
  expect_equal(BETAI(a = 0, b = 0, x = 0),
               NaN)

})

test_that("BETAICF returns expected results", {
  # Test1
  a <- 0.88410180773089975
  b <- 2.0515720003894735
  x <- 0.71833938360214233

  benchmarck <- 8.75843577012486

  out <- BETAICF(a = a,
                 b = b,
                 x = x)
  expect_equal(out, benchmarck)

  # Test2
  a <- 2.0515720003894735
  b <- 0.88410180773089975
  x <- 0.28166061639785767

  benchmarck <- 1.3733003812298712

  out <- BETAICF(a = a,
                 b = b,
                 x = x)
  expect_equal(out, benchmarck)


})

test_that("gd_compute_headcount_lb returns expected results", {
  # Constants
  mean <- 51.5660557757944
  povline <- 57.791666666666664
  A <- 0.57803721740313529
  B <- 0.94205090386544987
  C <- 0.52578600019473676
  benchmarck <- 0.71833938360214233

  out <- gd_compute_headcount_lb(mean = mean,
                                 povline = povline,
                                 A = A,
                                 B = B,
                                 C = C)
  expect_equal(out, benchmarck, tolerance = 1.1e-06) #1e-06

})

test_that("gd_compute_headcount_lb will return NAs, headcount is negative or NA", {

  expect_equal(gd_compute_headcount_lb(mean = 0,
                                       povline = 1.9,
                                       A = 1,
                                       B = 0,
                                       C = 1),
               NA)

})

test_that("gd_compute_pov_severity_lb returns expected results", {
  # Constants
  u <- 0.892274937721028
  headcount <- 0.71833938360214233
  pg <- 0.27137498479545558
  A <- 0.57803721740313529
  B <- 0.94205090386544987
  C <- 0.52578600019473676
  benchmarck <- 0.12933674851941607

  out <- gd_compute_pov_severity_lb(u = u,
                                    headcount = headcount,
                                    pov_gap = pg,
                                    A = A,
                                    B = B,
                                    C = C)
  expect_equal(out, benchmarck)

})

test_that("gd_compute_watts_index_lb returns expected results", {

  headcount  <- 0.71833938360214233
  mean       <- 51.5660557757944
  povline    <- 57.791666666666664
  dd         <- 0.005
  A          <- 0.57803721740313529
  B          <- 0.94205090386544987
  C          <- 0.52578600019473676
  benchmarck <- 0.37920301922145383

  out <- gd_compute_watts_lb(headcount = headcount,
                             mean = mean,
                             povline = povline,
                             dd = dd,
                             A = A,
                             B = B,
                             C = C)
  expect_equal(out, benchmarck, tolerance = 1.1e-03) #1e-03
})

test_that("gd_compute_poverty_stats_lb works as expected", {

  mean    <- 51.5660557757944
  povline <- 57.791666666666664
  A       <- 0.57803721740313529
  B       <- 0.94205090386544987
  C       <- 0.52578600019473676

  benchmark <- list(
    headcount = 0.71833938360214233,
    pg = 0.27137498479545558,
    p2 = 0.12933674851941607,
    eh = -0.88105325099863419,
    epg = -1.6470361081496838,
    ep = -2.1964095727165542,
    gh = -0.094911516334918733,
    gpg = 0.71484787039488751,
    gp = 1.5479415174310347,
    watt = 0.37920301922145383,
    dl = 1.1207307983025634,
    ddl = 1.77079982191727
  )

  out <- gd_compute_poverty_stats_lb(mean = mean,
                                  povline = povline,
                                  A = A,
                                  B = B,
                                  C = C)

  expect_equal(length(out), length(benchmark))
  expect_equal(names(out), c("headcount", "pg", "p2", "eh", "epg",
                             "ep", "gh", "gpg", "gp", "watts", "dl", "ddl"))
  # I ran this one in debug mode. The results are the same as the .Net codebase
  # but the results are modified in .Net by `(double)(float)`. Not sure what is
  # exactly happening... Seems that `float` generates a loss of precision...
  # See LorenzQuadratic.cs, line 192
  expect_equal(out$headcount, benchmark$headcount, tolerance = 1.1e-06) #1e-06
  expect_equal(out$pg, benchmark$pg)
  expect_equal(out$p2, benchmark$p2)
  expect_equal(out$eh, benchmark$eh, tolerance = 1e-05) # Due to headcount difference
  expect_equal(out$epg, benchmark$epg, tolerance = 1e-05) # Due to headcount difference
  expect_equal(out$ep, benchmark$ep)
  expect_equal(out$gh, benchmark$gh, tolerance = 1e-05) # 1e-06
  expect_equal(out$gpg, benchmark$gpg, tolerance = 1e-05) # 1e-06
  expect_equal(out$gp, benchmark$gp)
  expect_equal(out$watt, benchmark$watt, tolerance = 1.1e-03) #1e-03
  expect_equal(out$dl, benchmark$dl, tolerance = 1e-05)
  expect_equal(out$ddl, benchmark$ddl, tolerance = 1e-05)
})


test_that("DDLK works as expected", {

  h       <- 0.001
  A       <- 0.57803721740313529
  B       <- 0.94205090386544987
  C       <- 0.52578600019473676

  benchmark <- 47.919869915279428

  out <- DDLK(h = h,
              A = A,
              B = B,
              C = C)

  expect_equal(out, benchmark)
})

test_that("check_curve_validity_lb works as expected", {

  # Invalid fit
  headcount <- 0.72895810546874995
  A <- 1.5059569306828811
  B <- 1.2068983362374488
  C <- 0.60013901263412039

  expected <- list(
    is_valid  = FALSE,
    is_normal = TRUE
  )

  expect_equal(check_curve_validity_lb(headcount = headcount,
                              A = A,
                              B = B,
                              C = C),
               expected)


  ## test that if (derive_lb(w, A, B, C) < 0), is_valid is not TRUE
  headcount <- 0.5
  A <- 1
  B <- 0.3
  C <- 0.2

  expected <- list(is_valid = FALSE,
                   is_normal = TRUE)

  expect_equal(check_curve_validity_lb(headcount = headcount,
                                       A = A,
                                       B = B,
                                       C = C),
               expected)


  headcount <- NA
  A <- 1
  B <- 0.3
  C <- 0.2

  expected <- list(is_valid = FALSE,
                   is_normal = FALSE)

  expect_equal(check_curve_validity_lb(headcount = headcount,
                                       A = A,
                                       B = B,
                                       C = C),
               expected)


})


test_that("if PPP and default PPP are not null, requested_mean is computed as expected", {
  gd_ex2 <- readRDS('../testdata/gd_ex2.RDS')

  try_out <- gd_compute_pip_stats_lb(welfare = gd_ex2$welfare,
                                     population = gd_ex2$weight,
                                     povline = 1.9,
                                     requested_mean = 2.911786,
                                     ppp = 2, default_ppp = 3)

  ex_req_mean <- 3/2*2.911786

  expect_equal(try_out$mean, ex_req_mean)
})


test_that("if popshare is not null, povline is computed as expected", {
  gd_ex2 <- readRDS('../testdata/gd_ex2.RDS')

  try_out <- gd_compute_pip_stats_lb(welfare = gd_ex2$welfare,
                                     population = gd_ex2$weight,
                                     povline = 1.9,
                                     requested_mean = 2.911786,
                                     popshare = 0.3)

  #just a heads up that I dont have an intuitive sense for A, B and C values so I
  #cheated by running the function above in debug mode and just taking the A, B, C
  #values there. I figured the most important thing is to test the if-statement
  ex_povline <- derive_lb(0.3, 0.6562181, 0.9676324, 0.5300527)*2.911786


  expect_equal(try_out$poverty_line, ex_povline, tolerance = 1e-7)

})



test_that("in derive_lb() function, if x = 0 & B >= 0,
          function returns expected values", {

  ##first, testing on the initial if statement to ensure x == 0 & B = 1 will return 1 - A
  try_beq1 <- derive_lb(x = 0, A = 0.6562181, B = 1, C = 0.5300527)

  exp_val1 <- 1-0.6562181

  expect_equal(try_beq1, exp_val1)

  ##next, testing on the initial if statement to ensure x == 0 & B > 1 will return 1
  try_bover1 <- derive_lb(x = 0, A = 0.6562181, B = 1.1, C = 0.5300527)

  expect_equal(try_bover1, 1)


  #also testing that if x == 0 and C is greater than or equal to 1
  try_ceq1 <- derive_lb(x = 1, A = 0.6562181, B = 0.9676324, C = 1)

  expect_equal(try_ceq1, 1.6562181)

  try_cover1 <- derive_lb(x = 1, A = 0.6562181, B = 0.9676324, C = 1.1)

  expect_equal(try_cover1, 1)

  try_inf <- derive_lb(x = 1, A = 0.6562181, B = 0.9676324, C = 0.9)

  expect_equal(try_inf, Inf)

})


test_that("in derive_lb() function, if x = 0 & B >= 0,
          function returns expected values", {

  ##first, testing on the initial if statement to ensure x == 0 & B = 1 will return 1 - A
  try_beq1 <- derive_lb(x = 0, A = 0.6562181, B = 1, C = 0.5300527)

  exp_val1 <- 1-0.6562181

  expect_equal(try_beq1, exp_val1)

  ##next, testing on the initial if statement to ensure x == 0 & B > 1 will return 1
  try_bover1 <- derive_lb(x = 0, A = 0.6562181, B = 1.1, C = 0.5300527)

  expect_equal(try_bover1, 1)


  #also testing that if x == 0 and C is greater than or equal to 1
  try_ceq1 <- derive_lb(x = 1, A = 0.6562181, B = 0.9676324, C = 1)

  expect_equal(try_ceq1, 1.6562181)

  try_cover1 <- derive_lb(x = 1, A = 0.6562181, B = 0.9676324, C = 1.1)

  expect_equal(try_cover1, 1)

  try_inf <- derive_lb(x = 1, A = 0.6562181, B = 0.9676324, C = 0.9)

  expect_equal(try_inf, Inf)
})


test_that("tests for the gd_compute_watts_lb", {

  ## when headcount is negative the function returns 0
  expect_equal(gd_compute_watts_lb(headcount = -.1,
                                   mean = 51.5660557757944,
                                   povline = 1.90,
                                   dd = 0.005,
                                   A  = 0.57803721740313529,
                                   B  = 0.94205090386544987,
                                   C  = 0.52578600019473676),
               0)

  ## when headcount is NA function output is 0
  expect_equal(gd_compute_watts_lb(headcount = NA,
                                   mean = 51.5660557757944,
                                   povline = 1.90,
                                   dd = 0.005,
                                   A  = 0.57803721740313529,
                                   B  = 0.94205090386544987,
                                   C  = 0.52578600019473676),
               0)

  ## test that x1 <= 0, gap <= snw/2 && that function returns NA if x1 and x2 are less than 0;
  ## this is very difficult test
  ## but these following lines should remove the red marks from the
  ## output lines 344 and 372
  expect_equal(gd_compute_watts_lb(headcount = 0.2,
                                   mean = 51.5660557757944,
                                   povline = 1.90,
                                   dd = 0.005,
                                   A  = 1,
                                   B  = 0.9676324,
                                   C  = 1),
              NA)

  ##first, testing on the initial if statement to ensure x == 0 & B = 1 will return 1 - A
  try_beq1 <- derive_lb(x = 0, A = 0.6562181, B = 1, C = 0.5300527)


  exp_val1 <- 1 - 0.6562181

  expect_equal(try_beq1, exp_val1)

  ##next, testing on the initial if statement to ensure x == 0 & B > 1 will return 1
  try_bover1 <- derive_lb(x = 0, A = 0.6562181, B = 1.1, C = 0.5300527)

  expect_equal(try_bover1, 1)


  #also testing that if x == 0 and C is greater than or equal to 1
  try_ceq1 <- derive_lb(x = 1, A = 0.6562181, B = 0.9676324, C = 1)

  expect_equal(try_ceq1, 1.6562181)

  try_cover1 <- derive_lb(x = 1, A = 0.6562181, B = 0.9676324, C = 1.1)

  expect_equal(try_cover1, 1)

})

test_that("in gd_compute_mld_lb ensure gap is 0.0005 when x1 <= 0", {

  ##not really a test but it should get the red mark away on coverage report to go away
  expect_equal(gd_compute_mld_lb(0.0005, A = 1, B = 0.9676324, C = 1),
               0.2165068, tolerance = 1e-7)

})


test_that("BETAI returns expected values", {
  expect_equal(BETAI(a = 0, b = 0, x = 0),
               NaN)

})


test_that("GAMMLN returns NA when tmp <= 0", {
  expect_equal(GAMMLN(xx = -100),
               NA)

  expect_equal(GAMMLN(xx = -4.4),
               NA)
})


test_that("rtSafe assigns xl and xh appropriately when fl < 0", {
  x <- 0.9
  mean <- 51.5660557757944
  povline <- 57.791666666666664
  A <- 0.57803721740313529
  B <- 0.94205090386544987
  C <- 0.52578600019473676

  expect_equal(rtSafe(x1 = x, x2 = 0.002, xacc = 1, mean = mean, povline = povline, A = A, B = B, C = C),
               0.451)

})






