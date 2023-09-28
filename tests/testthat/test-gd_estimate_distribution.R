output_names <-
  c(
    "poverty_line",
    "mean",
    "median",
    "headcount",
    "poverty_gap",
    "poverty_severity",
    "watts",
    "gini",
    "mld",
    "polarization",
    "deciles",
    "spl",
    "spr"
  )

test_that("gd_estimate_distribution is working for all distribution types", {
  # Type 5
  welfare <- c(24.84, 35.8, 45.36, 55.1, 64.92, 77.08, 91.75, 110.64, 134.9,
               167.76, 215.48, 261.66, 384.97)
  population <- c(0.92, 2.47, 5.11, 7.9, 9.69, 15.24, 13.64, 16.99, 10, 9.78,
                  3.96, 1.81, 2.49)
  gd_type <- 5

  out <- gd_estimate_distribution(welfare = welfare,
                                  population = population,
                                  gd_type = gd_type,
                                  mean = 109.9,
                                  povline = 89)

  expect_equal(class(out), "list")
  expect_equal(names(out), output_names)
  expect_equal(out$headcount, 0.45061503)

  # Type 2
  welfare <- c(1.2, 2, 2.7, 3.6, 5, 6, 8.6, 11.4, 15.9, 43.7)
  population <- rep(10, 10)
  gd_type <- 2

  out <- gd_estimate_distribution(welfare = welfare,
                                  population = population,
                                  gd_type = gd_type,
                                  mean = 109.9,
                                  povline = 89)

  expect_equal(class(out), "list")
  expect_equal(names(out), output_names)
  expect_equal(out$headcount, 0.644837)

  # Type 1
  welfare <- c(0.305, 0.732, 1.226, 1.77, 2.36, 2.993, 3.667, 4.379, 5.129,
               5.914, 10.369, 15.752, 22.148, 29.705, 38.748, 49.967, 64.962,
               100)
  population <- c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 30, 40, 50, 60, 70, 80,
                  90, 100)
  gd_type <- 1

  out <- gd_estimate_distribution(welfare = welfare,
                                  population = population,
                                  gd_type = gd_type,
                                  mean = 109.9,
                                  povline = 89)

  expect_equal(class(out), "list")
  expect_equal(names(out), output_names)
  expect_equal(out$headcount, 0.58567622)

})

test_that("gd_estimate_distribution is throwing an error when bad inputs are submitted", {
  # Vectors of unequal length
  welfare <- c(24.84, 35.8, 45.36, 55.1, 64.92, 77.08, 91.75, 110.64, 134.9,
               167.76, 215.48, 261.66)
  population <- c(0.92, 2.47, 5.11, 7.9, 9.69, 15.24, 13.64, 16.99, 10, 9.78,
                  3.96, 1.81, 2.49)
  gd_type <- 5

  expect_error(gd_estimate_distribution(welfare = welfare,
                                        population = population,
                                        gd_type = gd_type,
                                        mean = 109.9,
                                        povline = 89))

  # Vectors of wrong type
  welfare <- c("1", "2", "3", "4", "5")
  population <- c(0.92, 2.47, 5.11, 7.9, 9.69)
  gd_type <- 5

  expect_error(gd_estimate_distribution(welfare = welfare,
                                        population = population,
                                        gd_type = gd_type,
                                        mean = 109.9,
                                        povline = 89),
               "non-numeric argument to binary operator")

  welfare <- c(0.92, 2.47, 5.11, 7.9, 9.69)
  population <- c("1", "2", "3", "4", "5")
  gd_type <- 5

  expect_error(gd_estimate_distribution(welfare = welfare,
                                        population = population,
                                        gd_type = gd_type,
                                        mean = 109.9,
                                        povline = 89))


  # Incorrect data type
  welfare <- c(24.84, 35.8, 45.36, 55.1, 64.92, 77.08, 91.75, 110.64, 134.9,
               167.76, 215.48, 261.66, 384.97)
  population <- c(0.92, 2.47, 5.11, 7.9, 9.69, 15.24, 13.64, 16.99, 10, 9.78,
                  3.96, 1.81, 2.49)
  gd_type <- 0

  expect_error(gd_estimate_distribution(welfare = welfare,
                                        population = population,
                                        gd_type = gd_type,
                                        mean = 109.9,
                                        povline = 89),
               "Data must be of type 1, 2 or 5.")



})
