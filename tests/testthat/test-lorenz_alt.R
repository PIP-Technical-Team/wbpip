test_that("lorenz_alt() produces results as expected", {

  output <- lorenz_alt(welfare = 1:5, weight = 1:5)

  attr(output$welfare, "label") <- NULL
  attr(output$weight, "label") <- NULL
  attr(output$maxw, "label") <- NULL
  attr(output$cum_pop, "label") <- NULL
  attr(output$prop_pop, "label") <- NULL
  attr(output$cum_prop_pop, "label") <- NULL
  attr(output$cum_welfare, "label") <- NULL
  attr(output$lorenz,"label") <- NULL
  attr(output$cum_mean_welfare,"label") <- NULL
  attr(output$gn_lorenz,"label") <- NULL
  attr(output$prop_welfare,"label") <- NULL



  ##test that welfare, weights and maxw produces expected results
  expect_equal(output$welfare, (1:5)^2)
  expect_equal(output$weight, 1:5)
  expect_equal(output$maxw, 1:5)

  ##test that the cumulative variables are as expected
  expect_equal(output$cum_pop, cumsum(1:5))
  expect_equal(output$prop_pop, cumsum(1:5)/rep(15,5))
  expect_equal(output$cum_prop_pop, cumsum(cumsum(1:5)/rep(15,5)))

  ##test that welfare and lorenz computations are correct
  expect_equal(output$cum_welfare, cumsum((1:5)^2))
  expect_equal(output$lorenz, cumsum((1:5)^2)/55)
  expect_equal(output$prop_welfare, (1:5)^2/55)
  expect_equal(output$gn_lorenz, c(0.06666667, 0.33333333, 0.93333333, 2.00000000, 3.66666667), tol = 1e-5)
  expect_equal(output$cum_mean_welfare, cumsum((1:5)^2)/cumsum(1:5))

})
