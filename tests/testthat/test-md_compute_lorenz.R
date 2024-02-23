df <- readRDS(test_path("testdata", "md_lorenz3.rds"))

test_that("md_compute_lorenz() returns expected results", {
  out <- md_compute_lorenz(
    welfare = df$welfare,
    weight = df$weight
  )
  expect_equal(nrow(out), 100)
  expect_equal(out$lorenz_weight[100], 1)
  expect_equal(out$lorenz_welfare[100], 1)
  expect_equal(out$welfare[100], 169400)
})

test_that("md_compute_lorenz() returns same as old version",{

  new <- md_compute_lorenz(
    welfare = df$welfare,
    weight = df$weight
  )

  old <- old_md_compute_lorenz(
    welfare = df$welfare,
    weight = df$weight
  )
  expect_equal(new,old)
})
