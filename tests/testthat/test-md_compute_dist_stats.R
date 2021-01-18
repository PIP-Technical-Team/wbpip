data('md_ABC_2000_income')
md_ABC_2000_income <-
  md_clean_data(md_ABC_2000_income,
                welfare = 'welfare',
                weight = 'weight')$data

test_that('md_compute_dist_stats() returns correct results', {

  # Check that results are correct
  res <- md_compute_dist_stats(
    welfare = md_ABC_2000_income$welfare,
    weight = md_ABC_2000_income$weight
  )
  expect_equal(names(res),
               c('mean', 'median', 'gini', 'polarization', 'mld', 'quantiles'))
  expect_equal(res$mean, 3436146, tolerance = 1.5e-07)
  expect_equal(res$median, 2029658.5, tolerance = 1.5e-07)
  expect_equal(res$gini, 0.5147999, tolerance = 1.5e-07)
  expect_equal(res$polarization, 0.466789, tolerance = 1.5e-07)
  expect_equal(res$mld, 0.473784, tolerance = 1.5e-07)
  expect_equal(res$quantiles, tolerance = 1.5e-07,
               c(0.01343078, 0.02616405, 0.03535142, 0.04437412, 0.05416520 ,
                 0.06553887, 0.08320632, 0.10988798, 0.15703608,0.41084518))

  # Check that mean can be supplied
  # This also changes the result for polarization
  res2 <- md_compute_dist_stats(
    welfare = md_ABC_2000_income$welfare,
    weight = md_ABC_2000_income$weight,
    mean = 3436000
  )
  expect_equal(names(res2),
               c('mean', 'median', 'gini', 'polarization', 'mld', 'quantiles'))
  expect_equal(res2$mean, 3436000, tolerance = 1.5e-07)
  expect_equal(res2$median, 2029658.5, tolerance = 1.5e-07)
  expect_equal(res2$gini, 0.5147999, tolerance = 1.5e-07)
  expect_equal(res2$polarization, 0.4667191, tolerance = 1.5e-07)
  expect_equal(res2$mld, 0.473784, tolerance = 1.5e-07)
  expect_equal(res2$quantiles, tolerance = 1.5e-07,
               c(0.01343078, 0.02616405, 0.03535142, 0.04437412, 0.05416520 ,
                 0.06553887, 0.08320632, 0.10988798, 0.15703608,0.41084518))

})

