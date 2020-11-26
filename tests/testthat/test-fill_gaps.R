# Tests
test_that('fill_gaps() works correctly', {
  # Test that fill_gaps() returns a list with all poverty stats
  df <- data.frame(welfare = 1:1000, weight = rep(1, 1000))
  res <- fill_gaps(request_year = 2005,
                   survey_year = c(2000),
                   data = list(df0 = df),
                   predicted_request_mean = 5,
                   distribution_type = 'micro')
  expect_identical(names(res),
                   c('poverty_line','mean','median','headcount',
                     'poverty_gap','poverty_severity','watts',
                     'gini','mld','polarization','deciles'))
  # Test that fill_gaps() also works when there is no weight column
  df2 <- data.frame(welfare = 1:1000)
  res2 <- fill_gaps(request_year = 2005,
                    survey_year = c(2000),
                    data = list(df0 = df2),
                    predicted_request_mean = 5,
                    distribution_type = 'micro')
  expect_equal(res, res2)
})

# Extrapolation
test_that('fill_gaps() extrapolates correctly for micro', {
  deciles <- c(0.04013558, 0.05152938, 0.05902374, 0.06676945, 0.07472896,
               0.08423328, 0.09665738, 0.11156888, 0.14029230, 0.27506106 )
  res <- fill_gaps(request_year = 2005,
                   survey_year = c(2000),
                   data = list(df0 = md_DEF_2000_consumption),
                   predicted_request_mean = 6,
                   distribution_type = 'micro',
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
test_that('fill_gaps() interpolates correctly (monotonic) for micro', {
  deciles <- c(0.01399432, 0.02681976, 0.03636473, 0.04528415, 0.05617152,
               0.06945774, 0.08577961, 0.11189229, 0.15621016, 0.39802571)
  res <- fill_gaps(request_year = 2005,
                   survey_year = c(2000, 2010),
                   data = list(df0 = md_ABC_2000_income, df1 = md_ABC_2010_income),
                   predicted_request_mean = c(13, 13),
                   distribution_type = 'micro',
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
test_that('fill_gaps() interpolates correctly (non-monotonic) for micro', {
  deciles <- c(0.01399432, 0.02681976, 0.03636473, 0.04528415, 0.05617152,
               0.06945774, 0.08577961, 0.11189229, 0.15621016, 0.39802571)
  res <- fill_gaps(
    request_year = 2005,
    survey_year = c(2000, 2010),
    data = list(df0 = md_ABC_2000_income, df1 = md_ABC_2010_income),
    predicted_request_mean = c(14, 17),
    distribution_type = 'micro',
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

test_that('check_inputs_fill_gaps() catches input errors', {
  df <- data.frame(welfare = 1:1000, weight = rep(1, 1000))
  # Incorrect NA's
  expect_error(
    fill_gaps(request_year = NA,
              survey_year = c(2000, 2010),
              data = list(df0 = df, df1 = df),
              predicted_request_mean = c(5, 10),
              distribution_type = 'micro'))
  expect_error(
    fill_gaps(request_year = 2005,
              survey_year = c(2000, NA),
              data = list(df0 = df, df1 = df),
              predicted_request_mean = c(5, 10),
              distribution_type = 'micro'))
  expect_error(
    fill_gaps(request_year = 2005,
              survey_year = c(2000, 2010),
              data = list(df0 = df, df1 = df),
              predicted_request_mean = c(5, NA),
              distribution_type = 'micro'))
  expect_error(
    fill_gaps(request_year = 2005,
              survey_year = c(2000, 2010),
              data = list(df0 = df, df1 = df),
              predicted_request_mean = c(5, 10),
              distribution_type = 'micro',
              poverty_line = NA))
  # Non numeric classes
  expect_error(
    fill_gaps(request_year = '2005',
              survey_year = 2000,
              data = list(df0 = df),
              predicted_request_mean = 5,
              distribution_type = 'micro'))
  expect_error(
    fill_gaps(request_year = 2005,
              survey_year = '2000',
              data = list(df0 = df),
              predicted_request_mean = 5,
              distribution_type = 'micro'))
  expect_error(
    fill_gaps(request_year = 2005,
              survey_year = 2000,
              data = list(df0 = df),
              predicted_request_mean = '5',
              distribution_type = 'micro'))
  expect_error(
    fill_gaps(request_year = 2005,
              survey_year = 2000,
              data = list(df0 = df),
              predicted_request_mean = 5,
              distribution_type = 'micro',
              poverty_line = '1.9'))
  df2 <- data.frame(welfare = as.character(1:1000))
  expect_error(
    fill_gaps(request_year = 2005,
              survey_year = 2000,
              data = list(df0 = df2),
              predicted_request_mean = 5,
              distribution_type = 'micro'))
  expect_error(
    fill_gaps(request_year = 2005,
              survey_year = c(2000, 2010),
              data = list(df0 = df, df1 = df2),
              predicted_request_mean = c(5, 10),
              distribution_type = 'micro'))
  # More than one request year
  expect_error(
    fill_gaps(request_year = c(2000, 2005),
              survey_year = 2000,
              data = list(df0 = df),
              predicted_request_mean = 5,
              distribution_type = 'micro'))
  # More than one poverty line
  expect_error(
    fill_gaps(request_year = 2004,
              survey_year = 2000,
              data = list(df0 = df),
              predicted_request_mean = 5,
              distribution_type = 'micro',
              povert_line = c(1.9, 3.2)))
  # More than two survey years
  expect_error(
    fill_gaps(request_year = 2005,
              survey_year = c(2000, 2005, 2010),
              data = list(df0 = df, df1 = df),
              predicted_request_mean = c(5, 5),
              distribution_type = 'micro'))
  # More than two predicted means
  expect_error(
    fill_gaps(request_year = 2005,
              survey_year = c(2000, 2010),
              data = list(df0 = df, df1 = df),
              predicted_request_mean = c(5, 5, 5),
              distribution_type = 'micro'))
  # Unequal lengths for survey year and survey mean
  expect_error(
    fill_gaps(request_year = 2005,
              survey_year = c(2000, 2005),
              data = list(df0 = df),
              predicted_request_mean = 5,
              distribution_type = 'micro'))
  # Two survey means, but only one df
  expect_error(
    fill_gaps(request_year = 2005,
              survey_year = c(2000, 2005),
              data = list(df0 = df),
              predicted_request_mean = c(5, 10),
              distribution_type = 'micro'))
  # Incorrect column name for welfare
  df3 <- data.frame(welfare_ish = 1:1000)
  expect_error(
    fill_gaps(request_year = 2005,
              survey_year = 2000,
              data = list(df0 = df3),
              predicted_request_mean = 5,
              distribution_type = 'micro'))
  expect_error(
    fill_gaps(request_year = 2005,
              survey_year = c(2000, 2010),
              data = list(df0 = df, df1 = df3),
              predicted_request_mean = c(5, 10),
              distribution_type = 'micro'))
  # df0 not specified
  expect_error(
    fill_gaps(request_year = 2005,
              survey_year = 2000,
              data = list(df1 = df),
              predicted_request_mean = 5,
              distribution_type = 'micro'))
  # df1 not specified
  expect_error(
    fill_gaps(request_year = 2005,
              survey_year = 2000,
              data = list(df0 = df, df = df),
              predicted_request_mean = 5,
              distribution_type = 'micro'))
})

