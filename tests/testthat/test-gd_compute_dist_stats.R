data('gd_GHI_2009_income')
gd_ex1 <- readRDS('../testdata/gd_ex1.RDS')
gd_ex3 <- readRDS('../testdata/gd_ex3.RDS')

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

  # Test against example data
  res <- gd_compute_dist_stats(
    welfare = gd_ex1$welfare, population = gd_ex1$weight,
    mean = 330.5371)
  expect_equal(res$mean, 330.5371)
  expect_equal(res$median, 201.4286, tolerance = 1.5e-06)
  expect_equal(res$gini, 0.5135977, tolerance = 1.5e-06)
  expect_equal(res$polarization, 0.4212645, tolerance = 1.5e-06)
  expect_equal(res$deciles, tolerance = 2.5e-05,
               c(0.01609,0.02702, 0.03577, 0.04492, 0.05522,
                 0.0675, 0.0832, 0.1056, 0.1452, 0.4195))
  skip('mld doesn\'t match PCN value')
  expect_equal(res$mld, 0.4670367, tolerance = 1.5e-06)

})

# Implemented after fixing issue #120 (CAF 1992)
test_that('gd_compute_gini_lq returns correct results', {

  res <- gd_compute_dist_stats(gd_ex3$welfare, gd_ex3$weight, mean = 1.180057)
  expect_equal(res$gini, 0.61330745577995316)

})

