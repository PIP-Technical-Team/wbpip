md_lorenz1 <- readRDS(here::here("tests", "testthat", "testdata", "md_lorenz1.RDS"))
md_lorenz2 <- readRDS(here::here("tests", "testthat", "testdata", "md_lorenz2.RDS"))


test_that("md_compute_spl() computations are correct", {

  # Expectation 1 - ppp 2017, no median supplied ----
  out <- md_compute_spl(
    welfare = md_lorenz1$y,
    weight = md_lorenz1$lorenzW
  )
  expect_equal(
    out,
    max(1.15 + 0.5*get_weighted_median(
      x = md_lorenz1$y,
      weight = md_lorenz1$lorenzW
    ), 2.15
  )
  )

  # Expectation 2 - ppp 2011, no median supplied ----
  out <- md_compute_spl(
    welfare = md_lorenz1$y,
    weight = md_lorenz1$lorenzW,
    ppp_year = 2011
  )
  expect_equal(
    out,
    max(1 + 0.5*get_weighted_median(
      x = md_lorenz1$y,
      weight = md_lorenz1$lorenzW
    ), 1.9
    )
  )

  # Expectation 3 - Median supplied
  out1 <- md_compute_spl(
    welfare = md_lorenz1$y,
    weight = md_lorenz1$lorenzW
  )
  out2 <- md_compute_spl(
    weighted_median_welfare = get_weighted_median(x = md_lorenz1$y, weight = md_lorenz1$lorenzW)
  )
  expect_equal(
    out1,
    out2
  )

})


test_that("md_compute_spl() minimums work as expected", {


})

test_that("md_compute_spl() gives errors as expected", {


})



