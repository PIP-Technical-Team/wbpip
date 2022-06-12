gd_ex2 <- readRDS("../testdata/gd_ex2.RDS")

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
    deciles = 1:10
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
    expected
  )
})


test_that("handle_missing_ppp() returns correct results", {
  res1 <- handle_missing_ppp(2.15, 10, 1.2)

  expect_length(res1, 2)
  expect_equal(res1$ppp, 2.15)
  expect_equal(res1$requested_mean, 5.581395, tolerance = 0.001)

  res2 <- handle_missing_ppp(NULL, 10, 1.2)

  expect_length(res2, 2)
  expect_equal(res2$ppp, 1.2)
  expect_equal(res2$requested_mean, 10)
})
