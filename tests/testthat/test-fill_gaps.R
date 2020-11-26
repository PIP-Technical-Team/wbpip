# fill_gaps()
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

# check_inputs_fill_gaps()
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
              poverty_line = c(1.9, 3.2)))
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

