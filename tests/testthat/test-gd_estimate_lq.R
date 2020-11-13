lorenz_pop <- c(0.0005,
                0.0032,
                0.014799999999999999,
                0.0443,
                0.0991,
                0.257,
                0.4385,
                0.5938,
                0.7089,
                1)

lorenz_welfare <- c(5.824760527229386e-05,
                    0.000604029410841011,
                    0.0037949334793616948,
                    0.013988878652244477,
                    0.036992164583098786,
                    0.12140708906131342,
                    0.24531391873082081,
                    0.37446670169288321,
                    0.48753116241194566,
                    1)

test_that("name of components does not change", {
  fix_names_ct <- c("e", "m", "n", "k", "r", "s1", "s2", "A", "B", "C")

  ct <- get_components_lq(2, -1, 3)
  res_names_ct <- names(ct)

  expect_equal(fix_names_ct, res_names_ct)
})


test_that("create_functional_form_lq works as expected", {

  lp <- lorenz_pop[-10]
  lw <- lorenz_welfare[-10]


  bench <-
    list(
      ymean = 0.114957585555556,
      sst   = 0.0888974819545522,
      coef  = c(1.25594989467441, -1.81458290694242, -0.626218961114119),
      sse   = 1.26281675661875e-06,
      r2    = 0.999985794684744,
      mse   = 2.10469459436459e-07,
      se    = c(0.00868872352695941, 0.0288499753222786, 0.0474221281355051),
      coef_stat = structure(
        list(
          term      = c("x1", "x2", "x3"),
          estimate  = c(1.25594989467441, -1.81458290694242, -0.626218961114119),
          std.error = c(0.00868872352695941, 0.0288499753222786, 0.0474221281355051),
          statistic = c(144.549414051148,  -62.8972082877712, -13.2052057917929),
          p.value   = c(
            7.3939828756792e-12,
            1.08589492660935e-09,
            1.16479658121852e-05
          )
        ),
        row.names = c(NA, -3L),
        class = c("tbl_df", "tbl", "data.frame")
      ),
      fitness = structure(
        list(
          r.squared     = 0.999993923936883,
          adj.r.squared = 0.999990885905324,
          sigma         = 0.000458769505783088,
          statistic     = c(value = 329158.504334618),
          p.value       = c(value = 4.90697563916806e-16),
          df            = c(numdf = 3),
          logLik        = 58.2368099483759,
          AIC           = -108.473619896752,
          BIC           = -107.684721587407,
          deviance      = 1.26281675661875e-06,
          df.residual   = 6L,
          nobs          = 9L
        ),
        row.names = c(NA, -1L),
        class = c("tbl_df", "tbl", "data.frame")
      )
    )




  out <- create_functional_form_lq(welfare = lorenz_pop,
                                   population = lorenz_welfare)

  expect_true(is.list(out))
  expect_equal(length(out), 9)

  nout <- names(out)
  for (i in seq_along(nout)) {
    expect_equal(out[[nout[i]]], bench[[nout[i]]])
  }

})

test_that("gd_compute_dist_stats_lq works as expected", {
  mean = 51.5660557757944
  p0   = 0.5
  ct <- list(
    A    = 0.795981535745657,
    B    = -1.4445933880119242,
    C    = 0.14728191995919815,
    e    = -0.498670067692931,
    m    = -1.0970760862948583,
    n    = 0.851623285340541,
    r    = 1.3477796260474386
  )
  benchmark <- list(
    gini         = 0.32126464221602591,
    median       = 42.247782467994874,
    rmhalf       = 30.338461373077628,
    dcm          = 34.999705316492175,
    polarization = 0.22066218253919992,
    ris          = 0.29417085441813828,
    mld          = 0.1897890974403306,
    deciles      = c(0.037459351271787455,
                0.050102491249992831,
                0.060098424008397155,
                0.0689612882777707,
                0.077549299610190137,
                0.086590868400210574,
                0.097034440897180829,
                0.11071235633968635,
                0.13305852826044706,
                0.2784329516843369)
  )

  out <- gd_compute_dist_stats_lq(mean = mean,
                               p0 = p0,
                               ct)

  expect_equal(names(out), c("gini", "median", "rmhalf", "dcm", "polarization",
                             "ris", "mld", "deciles"))
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
  p0   <- 0.5
  dcm  <- 34.999705316492175

  ct   <- list(

    A    = 0.795981535745657,
    B    = -1.4445933880119242,
    C    = 0.14728191995919815,
    e    = -0.498670067692931,
    m    = -1.0970760862948583,
    n    = 0.851623285340541,
    r    = 1.3477796260474386

  )

  benchmark <- 0.22066218253919992

  out <- gd_compute_polarization_lq(mean = mean,
                                    p0 = p0,
                                    dcm = dcm,
                                    ct)

  expect_equal(out, benchmark)


})

test_that("compute_poverty_stats_lq works as expected", {

  mean <- 51.5660557757944
  povline <- 57.791666666666664
  ct <- list(

    A  = 0.795981535745657,
    B  = -1.4445933880119242,
    C  = 0.14728191995919815,
    e  = -0.498670067692931,
    m  = -1.0970760862948583,
    n  = 0.851623285340541,
    r  = 1.3477796260474386,
    s1 = -0.22612667749534146,
    s2 = 1.002393060455814
  )

  benchmark <- list(
    headcount = 0.76005810499191284,
    pg        = 0.27617606019159308,
    p2        = 0.12832887439632906,
    eh        = -0.87181309219603054,
    epg       = -1.7520781651553494,
    ep        = -2.3041920454886071,
    gh        = -0.093916119652440649,
    gpg       = 0.70353220826204077,
    gp        = 1.5363306438390838,
    watt      = 0.39088363448720104,
    dl        = 1.1207307654300092,
    ddl       = 1.691340795153677
  )

  out <- gd_compute_poverty_stats_lq(mean = mean,
                                     povline = povline,
                                     ct)

  expect_equal(length(out), length(benchmark))
  expect_equal(names(out), c("headcount", "pg", "p2", "eh", "epg",
                             "ep", "gh", "gpg", "gp", "watt", "dl", "ddl"))
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
  expect_equal(out$gh, benchmark$gh)
  expect_equal(out$gpg, benchmark$gpg)
  expect_equal(out$gp, benchmark$gp)
  expect_equal(out$watt, benchmark$watt)
  expect_equal(round(out$dl, 7), round(benchmark$dl, 7))
  expect_equal(round(out$ddl, 6), round(benchmark$ddl, 6))
})

test_that("gd_compute_fit_lq works as expected", {
  p <- c(0.00050000000000000001,
         0.00320000000000000015,
         0.01479999999999999892,
         0.04429999999999999910,
         0.09909999999999999365,
         0.25700000000000000622,
         0.43850000000000000089,
         0.59379999999999999449,
         0.70889999999999997460,
         1.00000000000000000000)
  l <- c(0.00005824760527229386,
         0.00060402941084101102,
         0.00379493347936169477,
         0.01398887865224447691,
         0.03699216458309878552,
         0.12140708906131342237,
         0.24531391873082081245,
         0.37446670169288320817,
         0.48753116241194566216,
         1.00000000000000000000)
  h <- 0.76005810499191284

  ct <- list(
    A    = 0.795981535745657,
    B    = -1.4445933880119242,
    C    = 0.14728191995919815,
    e    = -0.498670067692931,
    m    = -1.0970760862948583,
    n    = 0.851623285340541,
    r    = 1.3477796260474386
  )

  benchmark <- c(2.5301549524661934e-06, 0.0032215656135074632)

  out <- gd_compute_fit_lq(population = p,
                           welfare = l,
                           headcount = h,
                           ct)
  out <- unlist(out)
  out <- unname(out)

  expect_equal(out, benchmark)
})
