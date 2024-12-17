data("md_ABC_2000_income")
md_ABC_2000_income <-
  md_clean_data(md_ABC_2000_income,
    welfare = "welfare",
    weight = "weight",
    quiet = TRUE
  )$data

test_that("md_compute_dist_stats() returns correct results", {

  # Check that results are correct
  res <- md_compute_dist_stats(
    welfare = md_ABC_2000_income$welfare,
    weight = md_ABC_2000_income$weight)
  expect_equal(
    names(res),
    c("mean", "median", "gini", "polarization", "mld", "quantiles"))
  expect_equal(res$mean, 3436146, tolerance = 1.5e-07)
  expect_equal(res$median, 2029658.5, tolerance = 1.5e-07)
  expect_equal(res$gini, 0.5147999, tolerance = 1.5e-07)

  muL    <- fmean(x = md_ABC_2000_income$welfare[md_ABC_2000_income$welfare < res$median],
                  w = md_ABC_2000_income$weight[md_ABC_2000_income$welfare < res$median])
  mustar <- res$mean*(1 - res$gini)
  pol <- 2*(mustar - muL)/res$median

  expect_equal(res$polarization, pol, tolerance = 1.5e-07)
  expect_equal(res$mld, 0.473784, tolerance = 1.5e-07)
  lorenz <- md_compute_lorenz(welfare = md_ABC_2000_income$welfare,
                              weight  = md_ABC_2000_income$weight,
                              nbins   = 10)
  share <- diff(c(0, lorenz$lorenz_welfare))
  expect_equal(res$quantiles,
               share)

  # Check that mean can be supplied
  # This also changes the result for polarization
  res2 <- md_compute_dist_stats(
    welfare = md_ABC_2000_income$welfare,
    weight = md_ABC_2000_income$weight,
    mean = 3436000
  )
  expect_equal(
    names(res2),
    c("mean", "median", "gini", "polarization", "mld", "quantiles")
  )
  expect_equal(res2$mean, 3436000, tolerance = 1.5e-07)
  expect_equal(res2$median, 2029658.5, tolerance = 1.5e-07)
  expect_equal(res2$gini, 0.5147999, tolerance = 1.5e-07)
  expect_equal(res2$mld, 0.473784, tolerance = 1.5e-04)
  expect_equal(res2$quantiles,
               share)
  muL    <- fmean(x = md_ABC_2000_income$welfare[md_ABC_2000_income$welfare < res2$median],
                  w = md_ABC_2000_income$weight[md_ABC_2000_income$welfare < res2$median])
  mustar <- res2$mean*(1 - res2$gini)
  pol <- 2*(mustar - muL)/res2$median
  expect_equal(res2$polarization, pol , tolerance = 1.5e-07)
})
