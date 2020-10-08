lorenz <- readr::read_csv("../testdata/lorenz.csv")

test_that("md_compute_quantiles output is formatted as expected", {
  out <- md_compute_quantiles(lorenz)
  expect_equal(names(out), c("quantiles", "median"))
  expect_equal(length(out$quantiles), 10)
  expect_equal(length(out$median), 1)
  # FIX: Only work for deciles currently
  # out <- md_compute_quantiles(lorenz, n_quantile = 5)
  # expect_equal(length(out$quantiles), 5)
  # out <- md_compute_quantiles(lorenz, n_quantile = 20)

  expect_error(md_compute_quantiles(lorenz, n_quantile = 200))

})

test_that("md_compute_quantiles computations are correct", {
  out <- md_compute_quantiles(lorenz)

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
})
