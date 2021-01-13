data('gd_GHI_2009_income')

test_that('gd_compute_dist_stats() returns correct results', {

  # Test vs gd_compute_pip_stats()
  df <- gd_GHI_2009_income
  mean <- stats::weighted.mean(df$welfare, w = df$weight)
  df <- gd_clean_data(df, welfare = 'welfare', population = 'weight', gd_type = 5)
  res1 <- gd_compute_pip_stats(
    welfare = df$welfare, population = df$weight,
    povline = 1.9 * 365/12, requested_mean = mean)
  res2 <- gd_compute_dist_stats(
    welfare = df$welfare, population = df$weight,
    mean = mean)
  expect_equal(res1$mean, res2$mean)
  expect_equal(res1$median, res2$median)
  expect_equal(res1$gini, res2$gini)
  expect_equal(res1$mld, res2$mld)
  expect_equal(res1$polarization, res2$polarization)
  expect_equal(res1$deciles, res2$deciles)

})
