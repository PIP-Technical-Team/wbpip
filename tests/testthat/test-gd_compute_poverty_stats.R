data("gd_GHI_2009_income")

test_that("gd_compute_poverty_stats() returns correct results", {

  # Test vs gd_compute_pip_stats()
  df <- gd_GHI_2009_income
  df <- gd_clean_data(df,
    welfare = "welfare",
    population = "weight",
    gd_type = 5,
    quiet = TRUE
  )
  res1 <- gd_compute_pip_stats(
    welfare = df$welfare, population = df$weight,
    povline = 1.9 * 365 / 12, mean = 50,
    default_ppp = 1
  )
  res2 <- gd_compute_poverty_stats(
    welfare = df$welfare, population = df$weight,
    povline = 1.9 * 365 / 12, mean = 50,
    default_ppp = 1
  )
  expect_equal(res1$headcount, res2$headcount)
  expect_equal(res1$poverty_gap, res2$poverty_gap)
  expect_equal(res1$poverty_severity, res2$poverty_severity)
  expect_equal(res1$watts, res2$watts)
  expect_equal(res1$poverty_line, res2$poverty_line)
})
