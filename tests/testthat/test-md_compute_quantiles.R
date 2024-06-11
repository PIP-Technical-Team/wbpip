md_lorenz1 <- readRDS(test_path("testdata", "md_lorenz1.RDS"))
md_lorenz2 <- readRDS(test_path("testdata", "md_lorenz2.RDS"))
df <- readRDS(test_path("testdata", "md_lorenz3.rds"))



test_that("md_compute_quantiles_share() computations are correct", {
  out <- md_compute_quantiles_share(
    welfare = df$welfare,
    weight = df$weight
  )

  expect_equal(out, c(0.005920027,
                      0.016977475,
                      0.024752993,
                      0.033998374,
                      0.045897644,
                      0.057935898,
                      0.075664645,
                      0.104816656,
                      0.165985703,
                      0.468050586
  ))

})

test_that("md_compute_quantiles_share() takes lorenz argument", {
  lz <- md_compute_lorenz(
    welfare = df$welfare,
    weight = df$weight,
    nbins = 10
  )

  out <- md_compute_quantiles_share(
    lorenz = lz
  )

  expect_equal(out, c(0.005920027,
                      0.016977475,
                      0.024752993,
                      0.033998374,
                      0.045897644,
                      0.057935898,
                      0.075664645,
                      0.104816656,
                      0.165985703,
                      0.468050586
  ))

})



test_that("md_compute_quantiles() computations are correct", {
  out <- md_compute_quantiles(
    welfare = df$welfare,
    weight = df$weight
  )

  expect_equal(length(out), 10)

  expect_equal(out, c(44.00000,
                      73.33334,
                      106.33330,
                      139.33330,
                      183.33330,
                      233.75000,
                      315.00000,
                      445.50000,
                      770.00000,
                      169400.00000)
               )

})

test_that("md_compute_quantiles() takes lorenz argument", {
  lz <- md_compute_lorenz(
    welfare = df$welfare,
    weight = df$weight,
    nbins = 10
  )

  out <- md_compute_quantiles(
    lorenz = lz
  )

  expect_equal(length(out), 10)

  expect_equal(out, c(44.00000,
                      73.33334,
                      106.33330,
                      139.33330,
                      183.33330,
                      233.75000,
                      315.00000,
                      445.50000,
                      770.00000,
                      169400.00000)
  )

})


test_that("md_compute_median() computations are correct", {
  out <- md_compute_median(
    welfare = df$welfare,
    weight = df$weight
  )

  expect_equal(length(out), 1)

  expect_equal(out, 183.3333)

})

test_that("md_compute_median() takes lorenz argument", {
  lz <- md_compute_lorenz(
    welfare = df$welfare,
    weight = df$weight,
    nbins = 10
  )

  out <- md_compute_median(
    lorenz = lz
  )

  expect_equal(length(out), 1)

  expect_equal(out, 183.3333)

})





test_that("md_compute_cumulative_share", {

  output <- md_compute_cumulative_share(
    welfare    = md_GHI_2000_consumption$welfare,
    weight     = md_GHI_2000_consumption$weight,
    n          = 10
  )
  expect_length(
    output,10
  )
  expect_true( # different quantile algorithms, so not precise
    (output <= 1 & output >= 0) |> all()
  )

  output <- md_compute_cumulative_share(
    welfare    = c(1:10),
    weight     = rep(1, 10),
    n          = 10
  )
  expect_equal(
    output |> unname(),
    fcumsum(1:10)/sum(1:10)
  )


})

