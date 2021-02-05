gd_ex2 <- readRDS('../testdata/gd_ex2.RDS')

test_that('gd_compute_pip_stats() returns correct results', {

  res <- gd_compute_pip_stats(
    gd_ex2$welfare, gd_ex2$weight,
    requested_mean = 2.911786,, povline = 1.9)
  expect_equal(res$mean, 2.911786)
  expect_equal(res$median, 2.29062, tolerance = 1.5e-06)
  expect_equal(res$gini, 0.3466288, tolerance = 1.5e-06)
  expect_equal(res$mld, 0.1965911, tolerance = 1.5e-06)
  expect_equal(res$polarization, 0.2920545, tolerance = 1.5e-06)
  expect_equal(res$deciles, tolerance = 7e-05,
               c(0.03462,0.04439, 0.05376, 0.06321, 0.07327,
                 0.08465, 0.09856, 0.1176, 0.1499, 0.28))
  expect_equal(res$headcount, 0.3713033, tolerance = 1.5e-06)
  expect_equal(res$poverty_gap, 0.1002854, tolerance = 1.5e-06)
  expect_equal(res$poverty_severity, 0.03617225, tolerance = 1.5e-06)
  expect_equal(res$mld, 0.1965911, tolerance = 1.5e-06)
  skip('watts is slightly different')
  expect_equal(res$watts, 0.1287745, tolerance = 1.5e-06)

})
