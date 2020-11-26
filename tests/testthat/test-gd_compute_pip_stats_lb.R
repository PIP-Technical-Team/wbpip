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
  expect_equal(out$gini, benchmark$gini, tolerance = 1e-06)
  expect_equal(out$median, benchmark$median)
  expect_equal(out$rmhalf, benchmark$rmhalf)
  expect_equal(out$dcm, benchmark$dcm, tolerance = 1e-05) # Fails due to difference in gini
  expect_equal(out$polarization, benchmark$polarization, tolerance = 1e-07)
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
  expect_equal(out, benchmarck, tolerance = 1e-6)

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
  expect_equal(out, benchmarck, tolerance = 1e-06)

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
  expect_equal(out, benchmarck, tolerance = 1e-03)
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
  expect_equal(out$headcount, benchmark$headcount, tolerance = 1e-06)
  expect_equal(out$pg, benchmark$pg)
  expect_equal(out$p2, benchmark$p2)
  expect_equal(out$eh, benchmark$eh, tolerance = 1e-05) # Due to headcount difference
  expect_equal(out$epg, benchmark$epg, tolerance = 1e-05) # Due to headcount difference
  expect_equal(out$ep, benchmark$ep)
  expect_equal(out$gh, benchmark$gh, tolerance = 1e-06)
  expect_equal(out$gpg, benchmark$gpg, tolerance = 1e-06)
  expect_equal(out$gp, benchmark$gp)
  expect_equal(out$watt, benchmark$watt, tolerance = 1e-03)
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