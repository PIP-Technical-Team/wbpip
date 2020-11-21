# Load datasets
data('md_ABC_2000_income')
data('md_ABC_2010_income')
data('md_DEF_2000_consumption')

# Clean datasets
md_ABC_2000_income <-
  md_clean_data(md_ABC_2000_income, welfare = 'welfare', weight = 'weight')$data
md_ABC_2010_income <-
  md_clean_data(md_ABC_2010_income, welfare = 'welfare', weight = 'weight')$data
md_DEF_2000_consumption <-
  md_clean_data(md_DEF_2000_consumption, welfare = 'welfare', weight = 'weight')$data

# Tests
test_that('md_fill_gaps() works correctly', {
  # Test that md_fill_gaps() returns a list with all poverty stats
  df <- data.frame(welfare = 1:1000, weight = rep(1, 1000))
  res <- md_fill_gaps(request_year = 2005,
                      survey_year = c(2000),
                      data = list(df0 = df),
                      predicted_request_mean = 5,
                      poverty_line = 1.9)
  expect_identical(names(res),
                   c('poverty_line','mean','median','headcount',
                     'poverty_gap','poverty_severity','watts',
                     'gini','mld','polarization','deciles'))
  # Test that md_fill_gaps() also works when there is no weight column
  df2 <- data.frame(welfare = 1:1000)
  res2 <- md_fill_gaps(request_year = 2005,
                       survey_year = c(2000),
                       data = list(df0 = df2),
                       predicted_request_mean = 5,
                       poverty_line = 1.9)
  expect_equal(res, res2)
  df3 <- data.frame(welfare = 1:1000, weight = rep(1, 1000))
  df4 <- data.frame(welfare = 1:1000)
  res3 <- md_fill_gaps(request_year = 2005,
                       survey_year = c(2000, 2010),
                       data = list(df0 = df3, df1 = df4),
                       predicted_request_mean = c(5, 5),
                       poverty_line = 1.9)
  expect_false(isTRUE(all.equal(res, res3)))
})

# Extrapolation
test_that('md_fill_gaps() extrapolates correctly', {
  deciles <- c(0.04013558, 0.05152938, 0.05902374, 0.06676945, 0.07472896,
               0.08423328, 0.09665738, 0.11156888, 0.14029230, 0.27506106 )
  res <- md_fill_gaps(request_year = 2005,
                      survey_year = c(2000),
                      data = list(df0 = md_DEF_2000_consumption),
                      predicted_request_mean = 6,
                      poverty_line = 1.9)
  expect_equal(res$poverty_line, 1.9)
  expect_equal(res$mean, 6, tolerance = 1.5e-7)
  expect_equal(res$median, 4.726458, tolerance = 1.5e-7)
  expect_equal(res$headcount, 0.005424768, tolerance = 1.5e-7)
  expect_equal(res$poverty_gap, 0.001034757, tolerance = 1.5e-7)
  expect_equal(res$poverty_severity, 0.0002663465, tolerance = 1.5e-7)
  expect_equal(res$watts, 0.001201002, tolorance = 1.5e-7)
  expect_equal(res$gini, 0.31865, tolerance = 1.5e-7)
  expect_equal(res$mld, 0.1661658, tolerance = 1.5e-7)
  # expect_equal(res$polarization, NA)
  expect_equal(res$deciles, deciles, tolerance = 1.5e-7)
})

# Monotonic interpolation
test_that('md_fill_gaps() interpolates correctly (monotonic)', {
  deciles <- c(0.01399432, 0.02681976, 0.03636473, 0.04528415, 0.05617152,
               0.06945774, 0.08577961, 0.11189229, 0.15621016, 0.39802571)
  res <- md_fill_gaps(request_year = 2005,
                      survey_year = c(2000, 2010),
                      data = list(df0 = md_ABC_2000_income, df1 = md_ABC_2010_income),
                      predicted_request_mean = c(13, 13),
                      poverty_line = 1.9)
  expect_equal(res$poverty_line, 1.9)
  expect_equal(res$mean, 13, tolerance = 1.5e-7)
  expect_identical(res$median, NA)
  expect_equal(res$headcount, 0.0459062, tolerance = 1.5e-7)
  expect_equal(res$poverty_gap, 0.0161475, tolerance = 1.5e-7)
  expect_equal(res$poverty_severity, 0.008425631, tolerance = 1.5e-7)
  expect_equal(res$watts, 0.02101141, tolorance = 1.5e-7)
  expect_identical(res$gini, NA)
  expect_identical(res$mld, NA)
  expect_identical(res$polarization, NA)
  expect_identical(res$deciles, NA)
})

# Non-monotonic interpolation
test_that('md_fill_gaps() interpolates correctly (non-monotonic)', {
  deciles <- c(0.01399432, 0.02681976, 0.03636473, 0.04528415, 0.05617152,
               0.06945774, 0.08577961, 0.11189229, 0.15621016, 0.39802571)
  res <- md_fill_gaps(
    request_year = 2005,
    survey_year = c(2000, 2010),
    data = list(df0 = md_ABC_2000_income, df1 = md_ABC_2010_income),
    predicted_request_mean = c(14, 17),
    poverty_line = 1.9)
  expect_equal(res$poverty_line, 1.9)
  expect_equal(res$mean, 15.5, tolerance = 1.5e-7)
  expect_identical(res$median, NA)
  expect_equal(res$headcount, 0.03680496, tolerance = 1.5e-7)
  expect_equal(res$poverty_gap, 0.01232436, tolerance = 1.5e-7)
  expect_equal(res$poverty_severity, 0.006587024, tolerance = 1.5e-7)
  expect_equal(res$watts, 0.01543411, tolorance = 1.5e-7)
  expect_identical(res$gini, NA)
  expect_identical(res$mld, NA)
  expect_identical(res$polarization, NA)
  expect_identical(res$deciles, NA)
})
