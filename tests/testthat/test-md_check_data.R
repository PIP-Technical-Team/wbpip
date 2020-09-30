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

