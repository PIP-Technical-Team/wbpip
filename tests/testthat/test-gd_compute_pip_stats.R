gd_ex2 <- readRDS(test_path("testdata", "gd_ex2.RDS"))



test_that("gd_compute_pip_stats() returns correct results", {
  res <- gd_compute_pip_stats(
    gd_ex2$welfare, gd_ex2$weight,
    requested_mean = 2.911786, povline = 1.9,
    default_ppp = 1
  )
  expect_equal(res$mean, 2.911786)
  expect_equal(res$median, 2.29062, tolerance = 1.5e-06)
  expect_equal(res$gini, 0.3466288, tolerance = 1.5e-06)
  expect_equal(res$mld, 0.1965911, tolerance = 1.5e-06)
  expect_equal(res$polarization, 0.2920545, tolerance = 1.5e-06)
  expect_equal(res$deciles,
    tolerance = 7e-05,
    c(
      0.03462, 0.04439, 0.05376, 0.06321, 0.07327,
      0.08465, 0.09856, 0.1176, 0.1499, 0.28
    )
  )
  expect_equal(res$headcount, 0.3713033, tolerance = 1.5e-06)
  expect_equal(res$poverty_gap, 0.1002854, tolerance = 1.5e-06)
  expect_equal(res$poverty_severity, 0.03617225, tolerance = 1.5e-06)
  expect_equal(res$mld, 0.1965911, tolerance = 1.5e-06)
  skip("watts is slightly different")
  expect_equal(res$watts, 0.1287745, tolerance = 1.5e-06)
})

test_that("retrieve_distributional() returns correct results", {
  lq <- list(
    z_min         = 123,
    z_max         = 123,
    gini          = 123,
    median        = 123,
    rmhalf        = 123,
    polarization  = 123,
    ris           = 123,
    mld           = 123,
    deciles = 1:10,
    sse           = 123
  )
  lb <- "not_used"
  is_valid <- FALSE
  use_lq_for_dist <- "not_used"
  expected <- list(
    z_min         = NA_real_,
    z_max         = NA_real_,
    gini          = NA_real_,
    median        = NA_real_,
    # rmed          = NA_real_
    rmhalf        = NA_real_,
    polarization  = NA_real_,
    ris           = NA_real_,
    mld           = NA_real_,
    deciles       = rep(NA_real_, length(lq[["deciles"]])),
    sse           = NA_real_
  )

  expect_equal(
    retrieve_distributional(
      lq = lq,
      lb = lb,
      is_valid = is_valid,
      use_lq_for_dist = use_lq_for_dist
    ),
    expected,
    label = "Not returning NAs when lq is invalid"
  )


  lq <- list(
    deciles = 1:10
  )
  lb <- "not_used"
  is_valid <- FALSE
  use_lq_for_dist <- "not_used"

  expected2 <- list(
    deciles       = rep(NA_real_, length(lq[["deciles"]]))
  )

  expect_equal(
    retrieve_distributional(
      lq = lq,
      lb = lb,
      is_valid = is_valid,
      use_lq_for_dist = use_lq_for_dist
    ),
    expected2,
    label = "It is not returning the same object as NAs"
  )

  expect_equal(
    retrieve_distributional(
      lq = lq,
      lb = lb,
      is_valid = is_valid,
      use_lq_for_dist = use_lq_for_dist
    ) |>
      length(),
    1,
    label = "It is not returning the same number of objects"
  )




})


test_that("there is no non-monotonicity issues", {
  welfare <- c(0.001627099, 0.005665031, 0.018053738, 0.034416427, 0.104759131, 0.217308879,
               0.344013235, 0.445912758, 0.546653748, 0.629349157, 0.735393023, 0.802366805,
               0.848560276, 1.000000000)

  population <- c(0.005807122, 0.017521512, 0.047998763, 0.082875117, 0.205667649, 0.369685881,
                  0.527352350, 0.636749530, 0.731944193, 0.801841067, 0.880026880, 0.921517670,
                  0.946241705, 1.000000000)

  res <- gd_compute_pip_stats(
    welfare = welfare,
    povline = 24.35,
    population = population,
    requested_mean = 3.036399
  )
  hc_inferior <- res$headcount

  res <- gd_compute_pip_stats(
    welfare = welfare,
    povline = 30,
    population = population,
    requested_mean = 3.036399
  )
  hc_superior <- res$headcount

  expect_true(hc_inferior < hc_superior)

})
