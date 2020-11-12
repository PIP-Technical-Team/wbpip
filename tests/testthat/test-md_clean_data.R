context("md_clean_data: Conditions of arguments work correctly")
test_that("Arguments that don't exist are errors", {
  expect_error( md_clean_data(dt,
                              welfsdfe = "welfare",
                              weight  = "weight")
                )
})

test_that("Variables that don't exist in data are errors", {
  expect_error( md_clean_data(dt,
                              welfare = "bla",
                              weight  = "ble")
                )
})

context("md_clean_data: Elimination is done correctly")
nd <- md_clean_data(dt,
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


test_that("when weight is not provided, variable `weight` is created equal to 1",
          {

  dt2 <- md_clean_data(dt,
                welfare = "welfare")$data
  expect_equal(mean(dt2$weight), 1)

})
