data('gd_GHI_2009_income')

test_that('gd_compute_poverty_stats() returns correct results', {

  # Test vs gd_compute_pip_stats()
  df <- gd_GHI_2009_income
  mean <- stats::weighted.mean(df$welfare, w = df$weight)
  df <- gd_clean_data(df, welfare = 'welfare', population = 'weight', gd_type = 5)
  res1 <- gd_compute_pip_stats(
    welfare = df$welfare, population = df$weight,
    povline = 1.9 * 365/12, requested_mean = mean)
  res2 <- gd_compute_poverty_stats(
    welfare = df$welfare, population = df$weight,
    povline = 1.9 * 365/12, requested_mean = mean)
  expect_equal(res1$headcount, res2$headcount)
  expect_equal(res1$poverty_gap, res2$poverty_gap)
  expect_equal(res1$poverty_severity, res2$poverty_severity)
  expect_equal(res1$watts, res2$watts)
  expect_equal(res1$poverty_line, res2$poverty_line)
})

