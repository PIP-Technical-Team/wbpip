test.dt <- readRDS('tests/testdata/synthetic-microdata.RDS')
test.dt <- test.dt[[1]]$data

test_that('md_compute_poverty_stats() works as expected', {

  # how does the function handle unspecified weights?
  expect_equal(
    md_compute_poverty_stats(welfare = 1:100, povline = 10),
    md_compute_poverty_stats(welfare = 1:100, povline = 10, weight = rep(1,100))
  )

  # how does the function deal with missing values?
  # expect_equal(
  #   md_compute_poverty_stats(welfare = c(1,2,NA,4,5),
  #                            povline = 10,
  #                            weight = c(1,2,3,NA,5)),
  #   md_compute_poverty_stats(welfare = c(1,2,5),
  #                            povline = 10,
  #                            weight = c(1,2,5))
  # )

  # kind of redundant but how does it behave when no weights are specified and we have missing values?
  # expect_equal(
  #   md_compute_poverty_stats(welfare = c(1,2,NA,4,5), povline = 2),
  #   md_compute_poverty_stats(welfare = c(1,2,4,5), povline = 2)
  # )

  # how does it handle negative values?
  # expect_equal(
  #   md_compute_poverty_stats(welfare = c(1,2,-3,4,-5), povline = 2),
  #   md_compute_poverty_stats(welfare = c(1,2,4), povline = 2)
  # )

  # expect_equal(
  #   md_compute_poverty_stats(welfare = c(1,2,NA,4,5),
  #                            povline = 10,
  #                            weight = c(1,2,3,NA,5)),
  #   md_compute_poverty_stats(welfare = c(1,2,5),
  #                            povline = 10,
  #                            weight = c(1,2,5))
  # )

  # kind of redundant but how does it behave when no weights are specified and we have missing values?
  # expect_equal(
  #   md_compute_poverty_stats(welfare = c(1,2,NA,4,5), povline = 2),
  #   md_compute_poverty_stats(welfare = c(1,2,4,5), povline = 2)
  # )

  # how does it handle negative values?
  #   expect_equal(
  #     md_compute_poverty_stats(welfare = c(1,2,-3,4,-5), povline = 2),
  #     md_compute_poverty_stats(welfare = c(1,2,4), povline = 2)
  #   )
})

test_that('md_compute_poverty_stats() produces expected headcounts and poverty gap indices', {

  # does function return 0 headcount when all welfare is above poverty line?
  res <- md_compute_poverty_stats(welfare = 10:100, povline = 9, weight = 10:100)
  expect_equal(
    res$headcount,
    0
  )

  # does function return all as poor when all welfare values are below poverty line?
  pop <- 1:100
  res <- md_compute_poverty_stats(welfare = pop, povline = 101)
  expect_equal(
    res$headcount,
    length(pop)
  )

  # does poverty gap = 1 when welfare values are 0?
  res <- md_compute_poverty_stats(welfare = rep(0,10), povline = 12)
  expect_equal(
    res$poverty_gap,
    1
  )

  # does poverty gap = 0 when welfare values are at least the povline?
  res <- md_compute_poverty_stats(welfare = rep(21:30), povline = 15)
  expect_equal(
    res$poverty_gap,
    0
  )

  # does function produce results that match compute_poverty_stats in povcalnet
  res <- md_compute_poverty_stats(welfare = test.dt$welfare,
                                  povline = mean(test.dt$welfare),
                                  weight = test.dt$weight)
  res$headcount <- res$headcount/sum(test.dt$weight)
  pcn.values <- list()
  pcn.values[["headcount"]] <- 0.7333513
  pcn.values[["poverty_gap"]] <- 0.3957584
  pcn.values[["poverty_severity"]] <- 0.2534849
  pcn.values[["watts"]] <- 0.6899868

  expect_equal(res, pcn.values, tolerance = 1e-6)

})
