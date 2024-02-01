lorenz_pop <- c(
  0.0005,
  0.0032,
  0.014799999999999999,
  0.0443,
  0.0991,
  0.257,
  0.4385,
  0.5938,
  0.7089,
  1
)

lorenz_welfare <- c(
  5.824760527229386e-05,
  0.000604029410841011,
  0.0037949334793616948,
  0.013988878652244477,
  0.036992164583098786,
  0.12140708906131342,
  0.24531391873082081,
  0.37446670169288321,
  0.48753116241194566,
  1
)


test_that("create_functional_form_lq works as expected", {
  y <- c(
    5.8244212488773904e-05,
    0.00060366455931185,
    0.0037805319592489148,
    0.013793189926297255,
    0.035623744342555719,
    0.10666740778697173,
    0.18513500000774905,
    0.23424139101613642,
    0.24984452808920274
  )

  x1 <- c(
    -5.7997605272293861e-05,
    -0.000593789410841011,
    -0.003575893479361695,
    -0.012026388652244476,
    -0.027171354583098786,
    -0.055358089061313426,
    -0.053031668730820825,
    -0.021868261692883195,
    0.015008047588054352
  )

  x2 <- c(
    -5.8218481469657719e-05,
    -0.00060209651672631982,
    -0.0037387684638671417,
    -0.013369171327950046,
    -0.033326241072913695,
    -0.09020546717255587,
    -0.13774376536735589,
    -0.15210837422764917,
    -0.14192032137811739
  )
  x3 <- c(
    0.00044175239472770615,
    0.0025959705891589894,
    0.011005066520638304,
    0.030311121347755522,
    0.062107835416901208,
    0.13559291093868658,
    0.19318608126917919,
    0.21933329830711679,
    0.22136883758805431
  )

  out <- create_functional_form_lq(
    welfare = lorenz_welfare,
    population = lorenz_pop
  )

  expect_true(is.list(out))
  expect_equal(length(out$y), 9)
  expect_equal(dim(out$X), c(9, 3))
  expect_equal(out$y, y)
  expect_equal(out$X[,1], x1)
  expect_equal(out$X[,2], x2)
  expect_equal(out$X[,3], x3)
})

test_that("gd_compute_dist_stats_lq works as expected", {
  mean <- 51.5660557757944
  p0 <- 0.5
  A <- 0.795981535745657
  B <- -1.4445933880119242
  C <- 0.14728191995919815
  e <- -0.498670067692931
  m <- -1.0970760862948583
  n <- 0.851623285340541
  r <- 1.3477796260474386
  benchmark <- list(
    gini = 0.32126464221602591,
    median = 42.247782467994874,
    rmhalf = 30.338461373077628,
    dcm = 34.999705316492175,
    polarization = 0.22066218253919992,
    ris = 0.29417085441813828,
    mld = 0.1897890974403306,
    deciles = c(
      0.037459351271787455,
      0.050102491249992831,
      0.060098424008397155,
      0.0689612882777707,
      0.077549299610190137,
      0.086590868400210574,
      0.097034440897180829,
      0.11071235633968635,
      0.13305852826044706,
      0.2784329516843369
    )
  )

  out <- gd_compute_dist_stats_lq(
    mean = mean,
    p0 = p0,
    A = A,
    B = B,
    C = C,
    e = e,
    m = m,
    n = n,
    r = r
  )

  expect_equal(names(out), c(
    "gini", "median", "rmhalf", "dcm", "polarization",
    "ris", "mld", "deciles"
  ))
  expect_equal(length(out), length(benchmark))
  expect_equal(out$gini, benchmark$gini)
  expect_equal(out$median, benchmark$median)
  expect_equal(out$rmhalf, benchmark$rmhalf)
  expect_equal(out$dcm, benchmark$dcm)
  expect_equal(out$polarization, benchmark$polarization)
  expect_equal(out$ris, benchmark$ris)
  expect_equal(out$mld, benchmark$mld)
  expect_equal(out$deciles, benchmark$deciles)
})

test_that("gd_compute_polarization_lq works as expected", {
  mean <- 51.5660557757944
  p0 <- 0.5
  dcm <- 34.999705316492175
  A <- 0.795981535745657
  B <- -1.4445933880119242
  C <- 0.14728191995919815
  benchmark <- 0.22066218253919992

  out <- gd_compute_polarization_lq(
    mean = mean,
    p0 = p0,
    dcm = dcm,
    A = A,
    B = B,
    C = C
  )

  expect_equal(out, benchmark)
})

test_that("compute_poverty_stats_lq works as expected", {
  mean <- 51.5660557757944
  povline <- 57.791666666666664
  A <- 0.795981535745657
  B <- -1.4445933880119242
  C <- 0.14728191995919815
  e <- -0.498670067692931
  m <- -1.0970760862948583
  n <- 0.851623285340541
  r <- 1.3477796260474386
  s1 <- -0.22612667749534146
  s2 <- 1.002393060455814

  benchmark <- list(
    headcount = 0.76005810499191284,
    pg = 0.27617606019159308,
    p2 = 0.12832887439632906,
    eh = -0.87181309219603054,
    epg = -1.7520781651553494,
    ep = -2.3041920454886071,
    gh = -0.093916119652440649,
    gpg = 0.70353220826204077,
    gp = 1.5363306438390838,
    watt = 0.39088363448720104,
    dl = 1.1207307654300092,
    ddl = 1.691340795153677
  )

  out <- gd_compute_poverty_stats_lq(
    mean = mean,
    povline = povline,
    A = A,
    B = B,
    C = C,
    e = e,
    m = m,
    n = n,
    r = r,
    s1 = s1,
    s2 = s2
  )

  expect_equal(length(out), length(benchmark))
  expect_equal(names(out), c(
    "headcount", "pg", "p2", "eh", "epg",
    "ep", "gh", "gpg", "gp", "watts", "dl", "ddl"
  ))
  # I ran this one in debug mode. The results are the same as the .Net codebase
  # but the results are modified in .Net by `(double)(float)`. Not sure what is
  # exactly happening... Seems that `float` generates a loss of precision...
  # See LorenzQuadratic.cs, line 192
  expect_equal(round(out$headcount, 7), round(benchmark$headcount, 7))
  expect_equal(out$pg, benchmark$pg)
  expect_equal(out$p2, benchmark$p2)
  expect_equal(round(out$eh, 6), round(benchmark$eh, 6)) # Due to headcount difference
  expect_equal(round(out$epg, 7), round(benchmark$epg, 7)) # Due to headcount difference
  expect_equal(out$ep, benchmark$ep)
  expect_equal(out$gh, benchmark$gh, tolerance = 1.1e-07)
  expect_equal(out$gpg, benchmark$gpg)
  expect_equal(out$gp, benchmark$gp)
  expect_equal(out$watt, benchmark$watt)
  expect_equal(round(out$dl, 7), round(benchmark$dl, 7))
  expect_equal(round(out$ddl, 6), round(benchmark$ddl, 6))
})

test_that("gd_compute_pov_severity_lq() works as before the function update", {

  # Define objects -----
  mean          <- 51.5660557757944
  povline       <- 57.791666666666664
  A             <- 0.795981535745657
  B             <- -1.4445933880119242
  C             <- 0.14728191995919815
  e             <- -0.498670067692931
  m             <- -1.0970760862948583
  n             <- 0.851623285340541
  r             <- 1.3477796260474386
  s1            <- -0.22612667749534146
  s2            <- 1.002393060455814
  headcount     <- 0.76005810499191284
  pov_gap       <- 0.27617606019159308

  foo_benchmark <- 0.12832887439632906

  # function output
  foo <- gd_compute_pov_severity_lq(
    mean      = mean,
    povline   = povline,
    headcount = headcount,
    pov_gap   = pov_gap,
    A         = A,
    B         = B,
    C         = C,
    e         = e,
    m         = m,
    n         = n,
    r         = r,
    s1        = s1,
    s2        = s2
  )

  expect_equal(
    foo,
    foo_benchmark
  )

})

test_that("gd_compute_poverty_stats_lq works with negative headcount", {
  mean <- 51.5660557757944
  povline <- 57.791666666666664
  A <- 0.795981535745657
  B <- -1.4445933880119242
  C <- 0.14728191995919815
  e <- -0.498670067692931
  m <- -1.0970760862948583
  n <- 0.851623285340541
  r <- 1.3477796260474386
  s1 <- -0.22612667749534146
  s2 <- 1.002393060455814

  povline_neg <- -57.791666666666664 # Negative to test negative headcount

  benchmark <- list(
    headcount = 0,
    pg        = 0,
    p2        = 0,
    eh        = 0,
    epg       = 0,
    ep        = 0,
    gh        = 0,
    gpg       = 0,
    gp        = 0,
    watts     = 0,
    dl        = -1.120731,
    ddl       = 41.74948
  )

  out <- gd_compute_poverty_stats_lq(
    mean    = mean,
    povline = povline_neg,
    A       = A,
    B       = B,
    C       = C,
    e       = e,
    m       = m,
    n       = n,
    r       = r,
    s1      = s1,
    s2      = s2
  )

  out_original <- old_gd_compute_poverty_stats_lq(
    mean    = mean,
    povline = povline_neg,
    A       = A,
    B       = B,
    C       = C,
    e       = e,
    m       = m,
    n       = n,
    r       = r,
    s1      = s1,
    s2      = s2
  )


  expect_equal(length(out), length(benchmark))
  expect_equal(names(out),
               c("headcount", "pg", "p2", "eh", "epg",
                 "ep", "gh", "gpg", "gp", "watts", "dl", "ddl"))
  expect_equal(round(out$headcount, 7),
               round(benchmark$headcount, 7))
  expect_equal(out$pg,
               benchmark$pg)
  expect_equal(out$p2,
               benchmark$p2)
  expect_equal(round(out$eh, 6),
               round(benchmark$eh, 6)) # Due to headcount difference
  expect_equal(round(out$epg, 7),
               round(benchmark$epg, 7)) # Due to headcount difference
  expect_equal(out$ep,
               benchmark$ep)
  expect_equal(out$gh,
               benchmark$gh, tolerance = 1.1e-07)
  expect_equal(out$gpg,
               benchmark$gpg)
  expect_equal(out$gp,
               benchmark$gp)
  expect_equal(out$watts,
               benchmark$watts)
  expect_equal(round(out$dl, 5),
               round(benchmark$dl, 5))
  expect_equal(round(out$ddl, 5),
               round(benchmark$ddl, 5))
  expect_equal(out,
               benchmark,
               tolerance = 0.000001)
  expect_equal(out,
               out_original)
})




test_that("gd_compute_fit_lq works as expected", {
  p <- c(
    0.00050000000000000001,
    0.00320000000000000015,
    0.01479999999999999892,
    0.04429999999999999910,
    0.09909999999999999365,
    0.25700000000000000622,
    0.43850000000000000089,
    0.59379999999999999449,
    0.70889999999999997460,
    1.00000000000000000000
  )
  l <- c(
    0.00005824760527229386,
    0.00060402941084101102,
    0.00379493347936169477,
    0.01398887865224447691,
    0.03699216458309878552,
    0.12140708906131342237,
    0.24531391873082081245,
    0.37446670169288320817,
    0.48753116241194566216,
    1.00000000000000000000
  )
  h <- 0.76005810499191284
  A <- 0.795981535745657
  B <- -1.4445933880119242
  C <- 0.14728191995919815
  benchmark <- c(2.5301549524661934e-06, 0.0032215656135074632)

  out <- gd_compute_fit_lq(
    population = p,
    welfare = l,
    headcount = h,
    A = A,
    B = B,
    C = C
  )
  out <- unlist(out)
  out <- unname(out)

  expect_equal(out, benchmark)
})

test_that("gd_estimate_lq works as expected", {
  mean <- 1.50524
  povline <- 1.9
  p0 <- 0.5

  # Invalid fit
  A <- 0.78554131924835879
  B <- -1.9856022109519547
  C <- -0.30597079435662672

  expect_equal(
    gd_estimate_lq(
      mean = mean,
      povline = povline,
      p0 = p0,
      A = A,
      B = B,
      C = C
    ),
    empty_gd_compute_pip_stats_response
  )
})

test_that("gd_compute_watts_lq() gives correct results", {

  res <- gd_compute_watts_lq(
    headcount = 0.4,
    mean = 20,
    povline = 1.9,
    dd = 0.005,
    A = 0.2,
    B = 0.3,
    C = 0.4
  )
  expect_true(is.na(res))

  res <- gd_compute_watts_lq(
    headcount = 0.513180957,
    mean = 78.962,
    povline = 57.79166667,
    dd = 0.005,
    A = 0.7688156902,
    B = 0.9812052979,
    C = 0.4720329161
  )
  expect_equal(res, 0.4366290738)

})

mean      <- 51.5660557757944
povline   <- 57.791666666666664
A         <- 0.795981535745657
B         <- -1.4445933880119242
C         <- 0.14728191995919815
e         <- -0.498670067692931
m         <- -1.0970760862948583
n         <- 0.851623285340541
r         <- 1.3477796260474386
s1        <- -0.22612667749534146
s2        <- 1.002393060455814
headcount <- 0.76005810499191284
pov_gap   <- 0.27617606019159308

test_that("gd_compute_headcount works as expected", {

  # expected headcount ----
  benchmark <- 0.76005810499191284

  # headcount function ----
  out <- gd_compute_headcount_lq(
    mean    = mean,
    povline = povline,
    B       = B,
    m       = m,
    n       = n,
    r       = r
  )

  expect_equal(round(out, 7),
               round(benchmark, 7))

})


test_that("gd_compute_pov_gap_lq works as expected", {

  # expected pov gap
  benchmark <- 0.27617606019159308

  # output
  out <- gd_compute_pov_gap_lq(
    mean      = mean,
    povline   = povline,
    headcount = headcount,
    A         = A,
    B         = B,
    C         = C
  )

  expect_equal(out,
               benchmark)

})

test_that("gd_compute_pov_gap_lq works as expected when headcount negative", {

  headcount_neg <- -0.76005810499191284
  benchmark     <- 0

  out <- gd_compute_pov_gap_lq(
    mean      = mean,
    povline   = povline,
    headcount = headcount_neg,
    A         = A,
    B         = B,
    C         = C
  )

  expect_equal(out,
               benchmark)

})

test_that("gd_compute_pov_severity_lq works as expected", {

  benchmark <- 0.12832887439632906

  out <- gd_compute_pov_severity_lq(
    mean      = mean,
    povline   = povline,
    headcount = headcount,
    pov_gap   = pov_gap,
    A         = A,
    B         = B,
    C         = C,
    e         = e,
    m         = m,
    n         = n,
    r         = r,
    s1        = s1,
    s2        = s2
  )

  expect_equal(out,
               benchmark)

})

mean      <- 51.5660557757944
povline   <- 57.791666666666664
A         <- 0.795981535745657
B         <- -1.4445933880119242
C         <- 0.14728191995919815
e         <- -0.498670067692931
m         <- -1.0970760862948583
n         <- 0.851623285340541
r         <- 1.3477796260474386
s1        <- -0.22612667749534146
s2        <- 1.002393060455814
headcount <- 0.76005810499191284
pov_gap   <- 0.27617606019159308

test_that("gd_compute_headcount works as expected", {

  # expected headcount ----
  benchmark <- 0.76005810499191284

  # headcount function ----
  out <- gd_compute_headcount_lq(
    mean    = mean,
    povline = povline,
    B       = B,
    m       = m,
    n       = n,
    r       = r
  )

  expect_equal(round(out, 7),
               round(benchmark, 7))

})


test_that("gd_compute_pov_gap_lq works as expected", {

  # expected pov gap
  benchmark <- 0.27617606019159308

  # output
  out <- gd_compute_pov_gap_lq(
    mean      = mean,
    povline   = povline,
    headcount = headcount,
    A         = A,
    B         = B,
    C         = C
  )

  expect_equal(out,
               benchmark)

})

test_that("gd_compute_pov_gap_lq works as expected when headcount negative", {

  headcount_neg <- -0.76005810499191284
  benchmark     <- 0

  out <- gd_compute_pov_gap_lq(
    mean      = mean,
    povline   = povline,
    headcount = headcount_neg,
    A         = A,
    B         = B,
    C         = C
  )

  expect_equal(out,
               benchmark)

})

test_that("gd_compute_pov_severity_lq works as expected", {

  benchmark <- 0.12832887439632906

  out <- gd_compute_pov_severity_lq(
    mean      = mean,
    povline   = povline,
    headcount = headcount,
    pov_gap   = pov_gap,
    A         = A,
    B         = B,
    C         = C,
    e         = e,
    m         = m,
    n         = n,
    r         = r,
    s1        = s1,
    s2        = s2
  )

  expect_equal(out,
               benchmark)

})

test_that("value_at_lq works when x is a vector", {
  x <- c(
    0.00050000000000000001,
    0.00320000000000000015,
    0.01479999999999999892,
    0.04429999999999999910,
    0.09909999999999999365,
    0.25700000000000000622,
    0.43850000000000000089,
    0.59379999999999999449,
    0.70889999999999997460,
    1.00000000000000000000
  )

  # # Old values
  # A <- 0.795981535745657
  # B <- -1.4445933880119242
  # C <- 0.14728191995919815
  #
  # # Conditions of parameters in the corrected version of the paper
  # # m<0
  # # A+B+C+1>0
  # # C>=0
  # # A+C-1>=0  A+C>=1 (Not satisfied by old values)
  #
  # # We need `temp' to be negative --> (m * x^2) + (n * x) + (e^2) < 0
  # # if we assume x=1 and A+C=1
  # # we can choose A = 0.6 and C = 0.4
  # # also a B that satisfies:
  # # (4B^2 + 6B - 3 < 0) -> (-1.89 < B < 0.39)

  # # New values
  A <- 0.6
  B <- 0.3
  C <- 0.4

  # # Test that temp is negative in the last value:
  # e <- -(A + B + C + 1)
  # e2 <- e^2
  # m <- (B^2) - (4 * A)
  # n <- (2 * B * e) - (4 * C)
  # cond <- (m * x^2) + (n * x) + (e^2) < 0
  # cond  # Last values will be TRUE

  # # The benchmark is calculated using old unvectorized function
  # benchmark <- as.matrix(0,length(x))
  # for(i in 1:length(x)){
  #   benchmark[i]<-old_value_at_lq(x[i],A,B,C)
  # }
  # benchmark

  benchmark <- c(8.703071e-05,
                 5.595627e-04,
                 2.639177e-03,
                 8.294137e-03,
                 2.023636e-02,
                 6.603539e-02,
                 1.436005e-01,
                 2.384378e-01,
                 3.336276e-01,
                 1.000000e+00

  )

  out <- value_at_lq(
    x = x,
    A = A,
    B = B,
    C = C
  )

  expect_equal(out,
               benchmark,
               tolerance = 1.1e-04)
})

