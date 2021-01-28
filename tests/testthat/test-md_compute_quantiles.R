md_lorenz1 <- readRDS('../testdata/md_lorenz1.RDS')
md_lorenz2 <- readRDS('../testdata/md_lorenz2.RDS')

test_that("md_compute_quantiles() output is formatted as expected", {
  out <- md_compute_quantiles(
    lwelfare = md_lorenz1$lorenzY,
    lweight = md_lorenz1$lorenzW,
    percentile = md_lorenz1$y)
  expect_equal(names(out), c("quantiles", "median"))
  expect_equal(length(out$quantiles), 10)
  expect_equal(length(out$median), 1)
  # FIX: Only work for deciles currently
  # out <- md_compute_quantiles(lorenz, n_quantile = 5)
  # expect_equal(length(out$quantiles), 5)
  # out <- md_compute_quantiles(lorenz, n_quantile = 20)

  expect_error(md_compute_quantiles(
    lwelfare   = md_lorenz1$lorenzY,
    lweight    = md_lorenz1$lorenzW,
    percentile = md_lorenz1$y,
    n_quantile = 200))

})

test_that("md_compute_quantiles() computations are correct", {
  out <- md_compute_quantiles(
    lwelfare   = md_lorenz1$lorenzY,
    lweight    = md_lorenz1$lorenzW,
    percentile = md_lorenz1$y)
  expect_equal(out$median, 228.666687012)
  expect_equal(out$quantiles, c(0.00847950735425,
                                0.01795704602808,
                                0.02628152744606,
                                0.03568864262360,
                                0.04662169097793,
                                0.06072183506751,
                                0.07910940020320,
                                0.10750239876047,
                                0.16352959364197,
                                0.45410835789693))

  out <- md_compute_quantiles(
    lwelfare = md_lorenz2$lorenz_welfare,
    lweight = md_lorenz2$lorenz_weight,
    percentile = md_lorenz2$welfare)
  expect_equal(out$median, 7.105014)
  expect_equal(out$quantiles, c(0.00983246,
                                0.02195307,
                                0.03342455,
                                0.04495307,
                                0.05662774,
                                0.07048758,
                                0.08808485,
                                0.11349456,
                                0.15868695,
                                0.40245516))


})
