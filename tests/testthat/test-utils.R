test_that("get_decimal_year_value works", {
  res <- get_decimal_year_value(
        year= c(2003.67),
        value = c(1682.1, 1791.262)) #Values used from test-predict_request_year_mean.R

  out <- 1755.2385

  expect_equal(res,out,tolerance = 1e-3 )
})

test_that("get_weights is working", {
  res <- get_weights(
    year= c(2003.67)) #Value used from test-predict_request_year_mean.R
  out <- c(0.3299999999999,0.6700000000000)

  expect_equal(res,out)
})

test_that("check NA_Inf function works when NA values",{
  x <- c(
    0.00050000000000000001,
    0.00320000000000000015,
    0.01479999999999999892,
    NA,
    0.09909999999999999365,
    0.25700000000000000622,
    0.43850000000000000089,
    0.59379999999999999449,
    0.70889999999999997460,
    1.00000000000000000000
  )

  n <- c(
    0.00050000000000000001,
    0.00320000000000000015,
    0.01479999999999999892,
    Inf,
    0.09909999999999999365,
    0.25700000000000000622,
    0.43850000000000000089,
    0.59379999999999999449,
    0.70889999999999997460,
    1.00000000000000000000
  )

  expect_error(check_NA_Inf_values(x))
  expect_error(check_NA_Inf_values(n))
})

test_that("check negative function works",{
  x <- c(
    0.00050000000000000001,
    0.00320000000000000015,
    0.01479999999999999892,
    -0.04429999999999999910,
    0.09909999999999999365,
    0.25700000000000000622,
    0.43850000000000000089,
    0.59379999999999999449,
    0.70889999999999997460,
    1.00000000000000000000
  )

  expect_error(check_neg_values(x))
})
