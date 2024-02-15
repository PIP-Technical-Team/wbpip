# Data ------------------------------------------
## use md_GHI_2000_consumption


# _______________________________________________
# Tests
# _______________________________________________

test_that("md_quantile_values", {

  output_check <-
    c(
      "10%" = 3032.35026692369,
      "20%" = 3877.66291497121,
      "30%" = 4654.7337064404,
      "40%" = 5451.87437787381,
      "50%" = 6291.8766358101,
      "60%" = 7325.87435276559,
      "70%" = 8654.29982700615,
      "80%" = 10448.7385566442,
      "90%" = 13620.9052916984,
      "100%" = 97747.2265625
    )


  output <- md_quantile_values(
    welfare    = md_GHI_2000_consumption$welfare,
    weight     = md_GHI_2000_consumption$weight,
    n          = 10,
    format     = "atomic"
  )

  expect_length(
    output,10
  )
  expect_equal(output, output_check, tolerance = 1e-10)

  expect_message(
    output <- md_quantile_values(
      welfare    = c(0:100),
      n          = 10,
      format     = "atomic"
    )
  )
  expect_equal(
    output |> unname(),
    seq(from = 10, to = 100, by = 10)
  )

  output <- md_quantile_values(
    welfare    = md_GHI_2000_consumption$welfare,
    weight     = md_GHI_2000_consumption$weight,
    n          = 10,
    format     = "list"
  )
  expect_true(
    output |> is.list()
  )
  output <- md_quantile_values(
    welfare    = md_GHI_2000_consumption$welfare,
    weight     = md_GHI_2000_consumption$weight,
    n          = 10,
    format     = "dt"
  )
  expect_true(
    output |> is.data.table()
  )


})





test_that("md_welfare_share_at", {

  output <- md_welfare_share_at(
    welfare    = md_GHI_2000_consumption$welfare,
    weight     = md_GHI_2000_consumption$weight,
    n          = 10,
    format     = "atomic"
  )
  expect_length(
    output,10
  )
  expect_true( # different quantile algorithms, so not precise
    (output <= 1 & output >= 0) |> all()
  )

  output <- md_welfare_share_at(
    welfare    = c(1:100),
    weight     = rep(1, 100),
    n          = 10,
    format     = "atomic"
  )
  expect_equal(
    output |> unname(),
    seq(from = 0.1, to = 1, by = 0.1)
  )


  output <- md_quantile_values(
    welfare    = md_GHI_2000_consumption$welfare,
    weight     = md_GHI_2000_consumption$weight,
    n          = 10,
    format     = "list"
  )
  expect_true(
    output |> is.list()
  )
  output <- md_quantile_values(
    welfare    = md_GHI_2000_consumption$welfare,
    weight     = md_GHI_2000_consumption$weight,
    n          = 10,
    format     = "dt"
  )
  expect_true(
    output |> is.data.table()
  )


})




test_that("md_quantile_welfare_share", {

  output <- md_quantile_welfare_share(
    welfare    = md_GHI_2000_consumption$welfare,
    weight     = md_GHI_2000_consumption$weight,
    n          = 10,
    format     = "atomic"
  )

  expect_length(
    output,10
  )
  expect_true( # should be increasing in shares
    all(diff(output) > 0)
  )
  expect_message(
    output <- md_quantile_welfare_share(
      welfare    = rep(c(100, 200), 100),
      n          = 2,
      format     = "atomic"
    )
  )
  expect_equal(
    round(output |> unname(), 2) |>
      as.double(),
    round(c(1/3, 2/3),2) |>
      as.double()
  )

  output <- md_quantile_welfare_share(
    welfare    = md_GHI_2000_consumption$welfare,
    weight     = md_GHI_2000_consumption$weight,
    n          = 10,
    format     = "list"
  )
  expect_true(
    output |> is.list()
  )
  output <- md_quantile_welfare_share(
    welfare    = md_GHI_2000_consumption$welfare,
    weight     = md_GHI_2000_consumption$weight,
    n          = 10,
    format     = "dt"
  )
  expect_true(
    output |> is.data.table()
  )


})





