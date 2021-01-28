df <- readr::read_rds("../testdata/md_lorenz3.rds")

test_that("md_compute_lorenz() returns expected results", {

  out <- md_compute_lorenz(welfare = df$welfare,
                           weight = df$weight)
  expect_equal(nrow(out), 100)
  expect_equal(out$lorenz_weight[100], 1)
  expect_equal(out$lorenz_welfare[100], 1)
  expect_equal(out$welfare[100], 169400)
})
