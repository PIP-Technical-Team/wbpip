# Lo_ad datasets
data('md_ABC_2000_income')
data('md_ABC_2010_income')
data('md_DEF_2000_consumption')
data('md_GHI_2000_income')
data('gd_GHI_2009_income')

# Clean datasets
md_ABC_2000_income <-
  md_clean_data(md_ABC_2000_income, welfare = 'welfare', weight = 'weight')$data
md_ABC_2010_income <-
  md_clean_data(md_ABC_2010_income, welfare = 'welfare', weight = 'weight')$data
md_DEF_2000_consumption <-
  md_clean_data(md_DEF_2000_consumption, welfare = 'welfare', weight = 'weight')$data
md_GHI_2000_income <-
  md_clean_data(md_GHI_2000_income, welfare = 'welfare', weight = 'weight')$data
gd_GHI_2009_income <- gd_clean_data(gd_GHI_2009_income, welfare = 'welfare',
                                    population = 'weight', gd_type = 5)

# Output format (named list)
test_that('fill_gaps() returns the correct output format', {
  # Test that fill_gaps() returns a list with all poverty stats
  df <- data.frame(welfare = 1:1000, weight = rep(1, 1000))
  res <- fill_gaps(request_year = 2005,
                   survey_year = 2000,
                   data = list(df0 = df),
                   default_ppp = 1,
                   predicted_request_mean = 5,
                   distribution_type = 'micro')
  expect_identical(names(res),
                   c('poverty_line','mean','median','headcount',
                     'poverty_gap','poverty_severity','watts',
                     'gini','mld','polarization','deciles'))
})

# Extrapolation
test_that('fill_gaps() extrapolates correctly for microdata', {
  deciles <- c(0.04013558, 0.05152938, 0.05902374, 0.06676945, 0.07472896,
               0.08423328, 0.09665738, 0.11156888, 0.14029230, 0.27506106 )
  res <- fill_gaps(
    request_year = 2005,
    survey_year = c(2000),
    data = list(df0 = md_DEF_2000_consumption),
    predicted_request_mean = 6,
    default_ppp = 1,
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

# Extrapolation
test_that('fill_gaps() extrapolates correctly for grouped data', {
  deciles <- c(0.02469825, 0.03389103, 0.04330000, 0.05330625, 0.06441968,
               0.07744013, 0.09384025, 0.11693318, 0.15714143, 0.33502981)
  res <- fill_gaps(
    request_year = 2005,
    survey_year = 2009,
    data = list(df0 = gd_GHI_2009_income),
    predicted_request_mean = 6,
    default_ppp = 1,
    distribution_type = 'group',
    poverty_line = 1.9)
  expect_equal(res$poverty_line, 1.9)
  expect_equal(res$mean, 6, tolerance = 1.5e-7)
  expect_equal(res$median, 4.231318, tolerance = 1.5e-7)
  expect_equal(res$headcount, 0.12776, tolerance = 1.5e-7)
  expect_equal(res$poverty_gap, 0.02657251, tolerance = 1.5e-7)
  expect_equal(res$poverty_severity, 0.007863721, tolerance = 1.5e-7)
  expect_equal(res$watts, 0.0318211, tolorance = 1.5e-7)
  expect_equal(res$gini, 0.4271266, tolerance = 1.5e-7)
  expect_equal(res$mld, 0.3057592, tolerance = 1.5e-7)
  expect_equal(res$polarization, 0.3790111, tolerance = 1.5e-7)
  expect_equal(res$deciles, deciles, tolerance = 1.5e-7)
})

# Monotonic interpolation
test_that('fill_gaps() interpolates correctly (monotonic) for microdata', {
  res <- fill_gaps(
    request_year = 2005,
    survey_year = c(2000, 2010),
    data = list(df0 = md_ABC_2000_income, df1 = md_ABC_2010_income),
    predicted_request_mean = c(13, 13),
    default_ppp = c(1, 1),
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

# Monotonic interpolation
test_that('fill_gaps() interpolates correctly (monotonic) for micro vs grouped data', {
  res <- fill_gaps(
    request_year = 2005,
    survey_year = c(2000, 2009),
    data = list(df0 = md_GHI_2000_consumption, df1 = gd_GHI_2009_income),
    predicted_request_mean = c(6, 6),
    default_ppp = 1,
    distribution_type = c('micro', 'group'),
    poverty_line = 1.9)
  expect_equal(res$poverty_line, 1.9)
  expect_equal(res$mean, 6, tolerance = 1.5e-7)
  expect_identical(res$median, NA)
  expect_equal(res$headcount, 0.09451118, tolerance = 1.5e-7)
  expect_equal(res$poverty_gap, 0.01874468, tolerance = 1.5e-7)
  expect_equal(res$poverty_severity, 0.005512714, tolerance = 1.5e-7)
  expect_equal(res$watts, 0.0224453, tolorance = 1.5e-7)
  expect_identical(res$gini, NA)
  expect_identical(res$mld, NA)
  expect_identical(res$polarization, NA)
  expect_identical(res$deciles, NA)
})

# Non-monotonic interpolation
test_that('fill_gaps() interpolates correctly (non-monotonic) for microdata', {
  res <- fill_gaps(
    request_year = 2005,
    survey_year = c(2000, 2010),
    data = list(df0 = md_ABC_2000_income, df1 = md_ABC_2010_income),
    predicted_request_mean = c(14, 17),
    default_ppp = c(1, 1),
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

# Non-monotonic interpolation
test_that('fill_gaps() interpolates correctly (non-monotonic) for micro vs grouped data', {
  res <- fill_gaps(
    request_year = 2005,
    survey_year = c(2000, 2009),
    data = list(df0 = md_GHI_2000_consumption, df1 = gd_GHI_2009_income),
    predicted_request_mean = c(4, 6),
    default_ppp = c(1, 1),
    distribution_type = c('micro', 'group'),
    poverty_line = 1.9)
  expect_equal(res$poverty_line, 1.9)
  expect_equal(res$mean, 5.111111, tolerance = 1.5e-7)
  expect_identical(res$median, NA)
  expect_equal(res$headcount, 0.1480512, tolerance = 1.5e-7)
  expect_equal(res$poverty_gap, 0.03320061, tolerance = 1.5e-7)
  expect_equal(res$poverty_severity, 0.01088438, tolerance = 1.5e-7)
  expect_equal(res$watts, 0.04089535, tolorance = 1.5e-7)
  expect_identical(res$gini, NA)
  expect_identical(res$mld, NA)
  expect_identical(res$polarization, NA)
  expect_identical(res$deciles, NA)
})

# fg_create_params
test_that('fg_create_params() is working correctly', {

  # One survey
  out <- fg_create_params(
    predicted_request_mean = 6,
    data = list(df0 = md_DEF_2000_consumption),
    default_ppp = 1,
    ppp = NULL,
    poverty_line = 1.9)
  expect_identical(names(out), c('params0'))
  expect_identical(names(out$params0),
                   c('welfare', 'population', 'povline',
                     'default_ppp', 'ppp', 'requested_mean'))
  expect_identical(out$params0$welfare, md_DEF_2000_consumption$welfare)
  expect_identical(out$params0$population, md_DEF_2000_consumption$weight)
  expect_identical(out$params0$povline, 1.9)
  expect_identical(out$params0$default_ppp, 1)
  expect_true(is.null(out$params0$ppp))
  expect_identical(out$params0$requested_mean, 6)

  # Two surveys
  out <- fg_create_params(
    predicted_request_mean = c(4, 6),
    data = list(df0 = md_GHI_2000_consumption, df1 = gd_GHI_2009_income),
    default_ppp = c(1, 1),
    ppp = NULL,
    poverty_line = 1.9)
  expect_identical(names(out), c('params0', 'params1'))
  expect_identical(names(out$params0),
                   c('welfare', 'population', 'povline',
                     'default_ppp', 'ppp', 'requested_mean'))
  expect_identical(names(out$params1),
                   c('welfare', 'population', 'povline',
                     'default_ppp', 'ppp', 'requested_mean'))
  expect_identical(out$params0$welfare, md_GHI_2000_consumption$welfare)
  expect_identical(out$params0$population, md_GHI_2000_consumption$weight)
  expect_identical(out$params0$povline, 1.9)
  expect_identical(out$params0$default_ppp, 1)
  expect_true(is.null(out$params0$ppp))
  expect_identical(out$params0$requested_mean, 4)
  expect_identical(out$params1$welfare, gd_GHI_2009_income$welfare)
  expect_identical(out$params1$population, gd_GHI_2009_income$weight)
  expect_identical(out$params1$povline, 1.9)
  expect_identical(out$params1$default_ppp, 1)
  expect_true(is.null(out$params1$ppp))
  expect_identical(out$params1$requested_mean, 6)

})

# check_inputs_fill_gaps()
test_that('check_inputs_fill_gaps() catches input errors', {
  df <- data.frame(welfare = 1:1000, weight = rep(1, 1000))
  # Incorrect NA's
  expect_error(
    fill_gaps(request_year = NA,
              survey_year = c(2000, 2010),
              data = list(df0 = df, df1 = df),
              default_ppp = 1,
              ppp = NULL,
              predicted_request_mean = c(5, 10),
              distribution_type = 'micro'))
  expect_error(
    fill_gaps(request_year = 2005,
              survey_year = c(2000, NA),
              data = list(df0 = df, df1 = df),
              default_ppp = 1,
              ppp = NULL,
              predicted_request_mean = c(5, 10),
              distribution_type = 'micro'))
  expect_error(
    fill_gaps(request_year = 2005,
              survey_year = c(2000, 2010),
              data = list(df0 = df, df1 = df),
              default_ppp = 1,
              ppp = NULL,
              predicted_request_mean = c(5, NA),
              distribution_type = 'micro'))
  expect_error(
    fill_gaps(request_year = 2005,
              survey_year = c(2000, 2010),
              data = list(df0 = df, df1 = df),
              default_ppp = 1,
              ppp = NULL,
              predicted_request_mean = c(5, 10),
              distribution_type = 'micro',
              poverty_line = NA))
  # Non numeric classes
  expect_error(
    fill_gaps(request_year = '2005',
              survey_year = 2000,
              data = list(df0 = df),
              default_ppp = 1,
              ppp = NULL,
              predicted_request_mean = 5,
              distribution_type = 'micro'))
  expect_error(
    fill_gaps(request_year = 2005,
              survey_year = '2000',
              data = list(df0 = df),
              default_ppp = 1,
              ppp = NULL,
              predicted_request_mean = 5,
              distribution_type = 'micro'))
  expect_error(
    fill_gaps(request_year = 2005,
              survey_year = 2000,
              data = list(df0 = df),
              default_ppp = 1,
              ppp = NULL,
              predicted_request_mean = '5',
              distribution_type = 'micro'))
  expect_error(
    fill_gaps(request_year = 2005,
              survey_year = 2000,
              data = list(df0 = df),
              default_ppp = 1,
              ppp = NULL,
              predicted_request_mean = 5,
              distribution_type = 'micro',
              poverty_line = '1.9'))
  df2 <- data.frame(welfare = as.character(1:1000))
  expect_error(
    fill_gaps(request_year = 2005,
              survey_year = 2000,
              data = list(df0 = df2),
              default_ppp = 1,
              ppp = NULL,
              predicted_request_mean = 5,
              distribution_type = 'micro'))
  expect_error(
    fill_gaps(request_year = 2005,
              survey_year = c(2000, 2010),
              data = list(df0 = df, df1 = df2),
              default_ppp = 1,
              ppp = NULL,
              predicted_request_mean = c(5, 10),
              distribution_type = 'micro'))
  # More than one request year
  expect_error(
    fill_gaps(request_year = c(2000, 2005),
              survey_year = 2000,
              data = list(df0 = df),
              default_ppp = 1,
              ppp = NULL,
              predicted_request_mean = 5,
              distribution_type = 'micro'))
  # More than one poverty line
  expect_error(
    fill_gaps(request_year = 2004,
              survey_year = 2000,
              data = list(df0 = df),
              default_ppp = 1,
              ppp = NULL,
              predicted_request_mean = 5,
              distribution_type = 'micro',
              poverty_line = c(1.9, 3.2)))
  # More than two survey years
  expect_error(
    fill_gaps(request_year = 2005,
              survey_year = c(2000, 2005, 2010),
              data = list(df0 = df, df1 = df),
              default_ppp = 1,
              ppp = NULL,
              predicted_request_mean = c(5, 5),
              distribution_type = 'micro'))
  # More than two predicted means
  expect_error(
    fill_gaps(request_year = 2005,
              survey_year = c(2000, 2010),
              data = list(df0 = df, df1 = df),
              default_ppp = 1,
              ppp = NULL,
              predicted_request_mean = c(5, 5, 5),
              distribution_type = 'micro'))
  # Unequal lengths for survey year and survey mean
  expect_error(
    fill_gaps(request_year = 2005,
              survey_year = c(2000, 2005),
              data = list(df0 = df),
              default_ppp = 1,
              ppp = NULL,
              predicted_request_mean = 5,
              distribution_type = 'micro'))
  # Two survey means, but only one df
  expect_error(
    fill_gaps(request_year = 2005,
              survey_year = c(2000, 2005),
              data = list(df0 = df),
              default_ppp = 1,
              ppp = NULL,
              predicted_request_mean = c(5, 10),
              distribution_type = 'micro'))
  # Incorrect column name for welfare
  df3 <- data.frame(welfare_ish = 1:1000)
  expect_error(
    fill_gaps(request_year = 2005,
              survey_year = 2000,
              data = list(df0 = df3),
              default_ppp = 1,
              ppp = NULL,
              predicted_request_mean = 5,
              distribution_type = 'micro'))
  expect_error(
    fill_gaps(request_year = 2005,
              survey_year = c(2000, 2010),
              data = list(df0 = df, df1 = df3),
              default_ppp = 1,
              ppp = NULL,
              predicted_request_mean = c(5, 10),
              distribution_type = 'micro'))
  # df0 not specified
  expect_error(
    fill_gaps(request_year = 2005,
              survey_year = 2000,
              data = list(df1 = df),
              default_ppp = 1,
              ppp = NULL,
              predicted_request_mean = 5,
              distribution_type = 'micro'))
  # df1 not specified
  expect_error(
    fill_gaps(request_year = 2005,
              survey_year = 2000,
              data = list(df0 = df, df = df),
              default_ppp = 1,
              ppp = NULL,
              predicted_request_mean = 5,
              distribution_type = 'micro'))
  # Incorrect distribution type
  expect_error(
    fill_gaps(request_year = 2005,
              survey_year = c(2000, 2010),
              data = list(df0 = df, df1 = df),
              default_ppp = 1,
              ppp = NULL,
              predicted_request_mean = c(5, 10),
              distribution_type = c('micro', 'micro2')))
})



