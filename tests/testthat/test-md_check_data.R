context("Conditions of arguments work correctly")
test_that("All the variables selected exist", {
  expect_error( md_check_data(dt,
                              welfsdfe = "welfare",
                              weight  = "weight")
                )
})

test_that("All arguments are valid exist", {
  expect_error( md_check_data(dt,
                              welfare = "bla",
                              weight  = "ble")
                )
})

context("Elimination is done correctly")
nd <- md_check_data(dt,
                    welfare = "welfare",
                    weight  = "weight")

test_that("NA are dropped in welfare", {
  expect_equal(15, nd$nna_welfare)
})
test_that("NA are dropped in weight", {
  expect_equal(17, nd$nna_weight)
})
test_that("Negative are dropped in welfare", {
  expect_equal(9, nd$nng_welfare)
})
test_that("Negative or zeros are dropped in weight", {
  expect_equal(11, nd$nng_weight)
})
