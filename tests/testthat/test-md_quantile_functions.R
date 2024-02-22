# Data ------------------------------------------
## use md_GHI_2000_consumption


# _______________________________________________
# Tests
# _______________________________________________

test_that("md_quantile_values", {

  output_check <-
    c(
      "10%" = 3031.85791016,
      "20%" = 3874.88134766,
      "30%" = 4655.96142578,
      "40%" = 5452.19921875,
      "50%" = 6291.76757812,
      "60%" = 7324.12695312,
      "70%" = 8655.76464844,
      "80%" = 10453.19140625,
      "90%" = 13698.80664062,
      "100%" = 97747.22656250
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

  # expect_message(
    output <- md_quantile_values(
      welfare    = c(0:100),
      n          = 10,
      format     = "atomic"
    )
  # )
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
    welfare    = c(1:10),
    weight     = rep(1, 10),
    n          = 10,
    format     = "atomic"
  )
  expect_equal(
    output |> unname(),
    fcumsum(1:10)/fsum(1:10)
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


#md_quantile_welfare_share

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
  # expect_message(
    output <- md_quantile_welfare_share(
      welfare    = rep(c(100, 200), 100),
      n          = 2,
      format     = "atomic"
    )
  # )
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





