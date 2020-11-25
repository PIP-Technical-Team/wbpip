benchmark <- readRDS('../testdata/synthetic-microdata.RDS')
benchmark <- benchmark[[1]]$data

test_that('md_compute_poverty_stats() matched expected results', {

  # does function return 0 headcount when all welfare is above poverty line?
  res <- md_compute_poverty_stats(welfare = 10:100, povline_lcu = 9, weight = 10:100)
  expect_equal(
    res$headcount,
    0
  )

  # does function return all as poor when all welfare values are below poverty line?
  pop <- 1:100
  res <- md_compute_poverty_stats(welfare = pop, povline_lcu = 101, weight = rep(1, 100))
  expect_equal(
    res$headcount,
    1
  )

  # does poverty gap = 1 when welfare values are 0?
  res <- md_compute_poverty_stats(welfare = rep(0, 10), povline_lcu = 12, weight = rep(1, 10))
  expect_equal(
    res$poverty_gap,
    1
  )

  # does poverty gap = 0 when welfare values are at least the povline?
  res <- md_compute_poverty_stats(welfare = 21:30, povline_lcu = 15, weight = rep(1, 10))
  expect_equal(
    res$poverty_gap,
    0
  )

  # does function produce results that match compute_poverty_stats in povcalnet
  res <- md_compute_poverty_stats(welfare = benchmark$welfare,
                                  povline_lcu = mean(benchmark$welfare),
                                  weight = benchmark$weight)

  expect_equal(res[["headcount"]], 0.7333513, tolerance = 1e-6)
  expect_equal(res[["poverty_gap"]], 0.3957584, tolerance = 1e-6)
  expect_equal(res[["poverty_severity"]], 0.2534849, tolerance = 1e-6)
  expect_equal(res[["watts"]], 0.6899868, tolerance = 1e-6)

})
