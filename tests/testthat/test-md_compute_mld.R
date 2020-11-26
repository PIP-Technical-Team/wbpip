df <- readr::read_rds("../testdata/microdata_lorenz.rds")

test_that("md_compute_mld returns consistent variable type", {

  out <- md_compute_mld(welfare = df$welfare,
                        weight = df$weight)
  expect_equal(length(out), 1)
  expect_true(is.numeric(out))
  # out <- md_compute_mld(welfare = rep(NA, 1000))
})

test_that("md_compute_mld returns expected results for know distribution", {

  out <- md_compute_mld(welfare = df$welfare,
                        weight = df$weight)
  expect_equal(out, 0.7502036651)
})


test_that("md_compute_mld returns expected results for perfect equality", {
  # Test for perfect equality
  out <- md_compute_mld(welfare = rep(2, 1000))
  expect_equal(out, 0)
})

test_that("md_compute_mld returns expected results for perfect inequality", {
  # Test for perfect inequality
  # MLD should be positive
  # out <- md_compute_mld(welfare = c(rep(0, 9999), 10000))
  # expect_true(out > 0)

  out <- md_compute_mld(welfare = c(rep(1, 9999), 10000))
  expect_true(out > 0)
})

test_that("md_compute_mld values change in the expected direction", {
  # Compare MLD values for two known distributions
  lower_expected_mld  <- md_compute_mld(welfare = c(rep(20, 999), 10000)) # Less inequal
  higher_expected_mld <- md_compute_mld(welfare = c(rep(20, 999), 100000))# More inequal
  expect_true(lower_expected_mld < higher_expected_mld)
})
