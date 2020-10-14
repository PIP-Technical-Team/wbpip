test.dt <- read.csv("tests/testdata/bra2003.csv")
PPP <- 1.658783
newPPP <- 1.658783
reqYearMean <- 531.2
QP = -1

daily_pl <- 1.9
monthly_pl <- daily_pl * 365 / 12
pl_LCU <- monthly_pl * 431.2037 / 531.2

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

  # does function produce results that match povcalnet outputs
  pcn.values <- list()
  pcn.values[["headcount"]] <- 0.06792263
  pcn.values[["poverty_gap"]] <- 0.02645899
  pcn.values[["poverty_severity"]] <- 0.01528384
  pcn.values[["watts"]] <- 0.03872916

  res <- md_compute_poverty_stats(welfare = test.dt$welfare,
                                  weight = test.dt$weight,
                                  povline = pl_LCU)
  res$headcount <- res$headcount/sum(test.dt$weight)

  expect_equal(pcn.values, res, tolerance = 1e-6)

})
