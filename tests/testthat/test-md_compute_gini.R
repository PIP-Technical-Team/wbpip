# Read in synthetic microdata
dl <- readRDS('../testdata/synthetic-microdata.RDS')

test_that('md_compute_gini() computations are correct', {

  # Test perfect equality
  expect_equal(
    md_compute_gini(welfare = rep(10, 100), weight = rep(1, 100)),
    0
  )

  # Test perfect inequality
  # Pass but slow to run
  # expect_equal(
  #   md_compute_gini(welfare = c(rep(0, 99999999), 100)),
  #   1
  # )

  # The theoritical value for perfect inequality is 1
  # But the md_compute_gini() is asymptotic and tends to 1 as the sample size
  # grows
  expect_equal(
    md_compute_gini(welfare = c(rep(0, 99), 100), weight = rep(1, 100)),
    0.99
  )
  expect_equal(
    md_compute_gini(welfare = c(rep(0, 999), 1000), weight = rep(1, 1000)),
    0.999
  )
  expect_equal(
    md_compute_gini(welfare = c(rep(0, 9999), 10000), weight = rep(1, 10000)),
    0.9999
  )

  # Test against pre-computed correct values
  lapply(dl, function(x) {
    df <- md_clean_data(x$data,
                        welfare = "welfare",
                        weight  = "weight")$data
    res <- md_compute_gini(welfare = df$welfare, weight = df$weight)
    expect_equal(res, x$stats$gini)
  })
})
