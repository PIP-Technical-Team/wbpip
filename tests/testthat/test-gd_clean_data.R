library(data.table)
library(tibble)

# Load example data
data('gd_GHI_2009_income')

# Re-create data from Datt 1998 and Group 5 in Povcalnet website.
dt_datt <- tibble::tribble(
  ~W,    ~X,      ~P,      ~L,
  0.92,   24.84,  0.0092, 0.00208,
  2.47,    35.8,  0.0339, 0.01013,
  5.11,   45.36,   0.085, 0.03122,
  7.9,    55.1,   0.164, 0.07083,
  9.69,   64.92,  0.2609, 0.12808,
  15.24,   77.08,  0.4133, 0.23498,
  13.64,   91.75,  0.5497, 0.34887,
  16.99,  110.64,  0.7196, 0.51994,
  10,   134.9,  0.8196,  0.6427,
  9.78,  167.76,  0.9174, 0.79201,
  3.96,  215.48,   0.957, 0.86966,
  1.81,  261.66,  0.9751, 0.91277,
  2.49,  384.97,       1,       1
)
data.table::setDT(dt_datt)
dt_datt[, R := (W * X) / sum(W * X)]

test_that('gd_clean_data() is working correctly', {

  # Test against example dataset (type5)
  dt <- gd_clean_data(
    gd_GHI_2009_income,
    welfare = 'welfare',
    population = 'weight',
    gd_type = 5)
  expect_identical(class(dt), class(gd_GHI_2009_income))
  expect_identical(names(dt), names(gd_GHI_2009_income))
  expect_equal(dt$weight, seq(0.1, 1, .1))
  expect_equal(dt$welfare,
               c(0.02292242, 0.05774930, 0.10232770, 0.15716421, 0.22252476,
                 0.29363491, 0.38883260, 0.50710468, 0.66958790, 1.00000000))

  # Test for type1 production data example
  welfare <- c(0.305, 0.732, 1.226, 1.77, 2.36, 2.993, 3.667, 4.379, 5.129,
               5.914,10.369, 15.752, 22.148, 29.705,38.748, 49.967, 64.962, 100)
  weight <- c(2, 4, 6,8, 10, 12, 14, 16, 18, 20, 30,	40,	50, 60,	70,	80,	90,	100)
  df <- data.frame(welfare = welfare, weight = weight)
  out <- gd_clean_data(df, welfare = 'welfare',
    population = 'weight', gd_type = 1)
  expect_equal(out$weight, c(seq(.02, .18, .02), seq(.2, 1, .1)))
  expect_equal(out$welfare,
               c(0.00305, 0.00732, 0.01226, 0.01770, 0.02360,
                 0.02993, 0.03667, 0.04379, 0.05129, 0.05914,
                 0.10369, 0.15752, 0.22148, 0.29705, 0.38748,
                 0.49967, 0.64962, 1.00000))

  # Test for type2 production data example
  welfare <- c(1.2, 2, 2.7, 3.6, 5, 6, 8.6, 11.4, 15.9, 43.7)
  weight <- rep(10, 10)
  df <- data.frame(welfare = welfare, weight = weight)
  out <- gd_clean_data(df, welfare = 'welfare',
     population = 'weight', gd_type = 2)
  expect_equal(out$weight, seq(.1, 1, .1))
  expect_equal(out$welfare,
               c(0.01198801, 0.03196803, 0.05894106, 0.09490509, 0.14485514,
                 0.20479520, 0.29070929, 0.40459540, 0.56343656, 1.00000000))


  # Data type must be of type 1, 2 or 5
  expect_error(gd_clean_data(
    gd_GHI_2009_income,
    welfare = 'welfare',
    population = 'weight',
    gd_type = 3))

  # Data can't contain NA's
  welfare <- c(gd_GHI_2009_income$welfare[1:9], NA)
  df <- data.frame(welfare = welfare,
                   population = rep(10, 10))
  expect_error(gd_clean_data(
    df,
    welfare = 'welfare',
    population = 'population',
    gd_type = 5))

})

test_that('gd_clean_data() standardizes correctly for Datt 1998 example', {

  cols <- c('population', 'welfare')

  dt1 <- gd_clean_data(dt_datt,
                       welfare = 'L',
                       population = 'P',
                       gd_type = 1)
  dt1 <- dt1[, c('P', 'L')]
  names(dt1) <- cols

  dt2 <- gd_clean_data(dt_datt,
                       welfare = 'R',
                       population = 'W',
                       gd_type = 2)
  dt2 <- dt2[, c('W', 'R')]
  names(dt2) <- cols

  dt5 <- gd_clean_data(dt_datt,
                       welfare = 'X',
                       population = 'W',
                       gd_type = 5)
  dt5 <- dt5[, c('W', 'X')]
  names(dt5) <- cols

  dt1a <- dt1[, lapply(.SD, round, digits = 4),
              .SDcols = cols]
  dt2a <- dt2[, lapply(.SD, round, digits = 4),
              .SDcols = cols]
  dt5a <- dt5[, lapply(.SD, round, digits = 4),
              .SDcols = cols]

  expect_equal(dt2, dt5)
  expect_equal(dt1a, dt2a)
  expect_equal(dt1a, dt5a)

})


test_that('gd_standardize_type1() is working correctly', {

  welfare <- c(0.305, 0.732, 1.226, 1.77, 2.36, 2.993, 3.667, 4.379, 5.129,
               5.914,10.369, 15.752, 22.148, 29.705,38.748, 49.967, 64.962, 100)
  weight <- c(2, 4, 6,8, 10, 12, 14, 16, 18, 20, 30,	40,	50, 60,	70,	80,	90,	100)
  res <- gd_standardize_type1(
    welfare = welfare, population = weight)
  expect_equal(res$population, c(seq(.02, .18, .02), seq(.2, 1, .1)))
  expect_equal(res$welfare,
               c(0.00305, 0.00732, 0.01226, 0.01770, 0.02360,
                 0.02993, 0.03667, 0.04379, 0.05129, 0.05914,
                 0.10369, 0.15752, 0.22148, 0.29705, 0.38748,
                 0.49967, 0.64962, 1.00000))
})

test_that('gd_standardize_type2() is working correctly', {

  welfare <- c(1.2, 2, 2.7, 3.6, 5, 6, 8.6, 11.4, 15.9, 43.7)
  weight <- rep(10, 10)
  res <- gd_standardize_type2(
    welfare = welfare, population = weight)
  expect_equal(res$population, seq(.1, 1, .1))
  expect_equal(res$welfare,
               c(0.01198801, 0.03196803, 0.05894106, 0.09490509, 0.14485514,
                 0.20479520, 0.29070929, 0.40459540, 0.56343656, 1.00000000))
})

test_that('standardize_type5() is working correctly', {

  welfare <- c(337, 518, 630, 736, 845, 980, 1142, 1344, 1645, 2387)
  weight <- rep(10, 10)
  res <- gd_standardize_type5(
    welfare = welfare, population = weight)
  expect_equal(res$population, seq(.1, 1, .1))
  expect_equal(res$welfare,
               c(0.03190080, 0.08093525, 0.14057175, 0.21024233, 0.29023097,
                 0.38299886, 0.49110186, 0.61832639, 0.77404392, 1.00000000))
})


