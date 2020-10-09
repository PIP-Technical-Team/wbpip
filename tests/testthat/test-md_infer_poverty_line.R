
context("uniform distribution of welfare")
test_that("multiplication works", {
  set.seed(10010)
  uv   <- 5
  ps   <- runif(1)
  size <- 1001
  expect_equal(md_infer_poverty_line(rep(uv, size),
                                     popshare = ps),
               uv)

  expect_equal(md_infer_poverty_line(rep(uv, size),
                                     sample(1:500, size, replace = TRUE),
                                     popshare = ps),
               uv)

})
