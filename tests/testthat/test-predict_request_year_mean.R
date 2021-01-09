test_that('predict_request_year_mean() returns correct results', {

  # Extrapolation of a single survey

  res <- predict_request_year_mean( # Dummy example
    survey_year = 2005,
    survey_mean = 2.0,
    proxy = list(value0 = 1350, req_value = 1500))
  expect_equal(res, 2.2222222)

  res <- predict_request_year_mean( # One-point same as survey year
    survey_year = 1996,
    survey_mean = 180.2398,
    proxy = list(value0 = 1121.8795, req_value = 1121.8795))
  expect_equal(res, 180.2398)

  res <- predict_request_year_mean( # One-point PCE
    survey_year = 2006,
    survey_mean = 31.95283,
    proxy = list(value0 = 11933.7557, req_value = 13203.1141)
  )
  expect_equal(res, 35.351558)

  res <- predict_request_year_mean( # One-point GDP
    survey_year = 2013.5,
    survey_mean = 53.3729,
    proxy = list(value0 = c(242.8459, 245.3267), req_value = 228.4325))
  expect_equal(res, 49.949977)

  # Interpolatation of two surveys (monotonic)

  res <- predict_request_year_mean( # Dummy example
    survey_year = c(2000, 2005),
    survey_mean =  c(2.0, 3.0),
    proxy = list(value0 = 1350, value1 = 1600, req_value = 1500))
  expect_equal(res, c(2.6, 2.6))

  res <- predict_request_year_mean( # Same direction interpolated PCE
    survey_year = c(1996, 2002),
    survey_mean = c(199.9253, 204.3373),
    proxy = list(value0 = 1006.0806, value1 = 1567.4769, req_value = 1336.3829))
  expect_equal(res, c(202.52114, 202.52114))

  res <- predict_request_year_mean( # Same direction interpolated GDP
    survey_year = c(2008.5, 2018.17),
    survey_mean = c(110.1658, 88.8960),
    proxy = list(value0 = c(3652.1536, 3549.5779),
                 value1 = c(3229.6196, 3088.4529) ,
                 req_value = 3549.5779))
  expect_equal(res, c(107.40578, 107.40578))

  # Interpolatation of two surveys (non-monotonic)

  res <- predict_request_year_mean( # Dummy example
    survey_year = c(2000, 2005),
    survey_mean =  c(2.0, 3.0),
    proxy = list(value0 = 1350, value1 = 1500, req_value = 1600))
  expect_equal(res, c(2.3703704, 3.20000))

  res <- predict_request_year_mean( # Non monotonic GDP
    survey_year = c(1996, 2003.67),
    survey_mean = c(2.216, 2.213),
    proxy = list(value0 = 1370.726, value1 = c(1682.1, 1791.262), req_value = 1682.1)
  )
  expect_equal(res, c(2.7193864, 2.1207871))

  res <- predict_request_year_mean( # Non monotonic PCE
    survey_year = c(2015, 2017.75),
    survey_mean = c(177.08428, 153.9266),
    proxy = list(value0 = 2206.4839, value1 = c(2306.04809, 2284.5361), req_value = 2260.0862))
  expect_equal(res, c(181.3862034, 151.9215873))

})

# check_inputs_predict_request_year_mean
test_that('check_inputs_predict_request_year_mean() catches input errors correctly', {

  # Survey year can't contain missing values
  expect_error(
    check_inputs_predict_request_year_mean(
    survey_year = c(2000, NA),
    survey_mean = c(100, 105),
    proxy = list(value0 = 1, value1 = 3, req_value = 2))
  )
  # proxy$req_value can't contain missing values
  expect_error(
    check_inputs_predict_request_year_mean(
      survey_year = c(2000, 2005),
      survey_mean = c(100, 105),
      proxy = list(value0 = 1, value1 = 3, req_value = NA))
  )
  # The survey mean and proxy value0 and value1
  # _can_ contain missing values (hence expect_silent)
  expect_silent(
    check_inputs_predict_request_year_mean(
    survey_year = c(2000, 2005),
    survey_mean = c(100, NA),
    proxy = list(value0 = 1, value1 = 3, req_value = 2))
    )
  expect_silent(
    check_inputs_predict_request_year_mean(
      survey_year = c(2000.5, 2005.5),
      survey_mean = c(100, NA),
      proxy = list(value0 = c(1, NA), value1 = c(3, NA), req_value = 2))
  )
  # Incorrect classes
  expect_error(
    check_inputs_predict_request_year_mean(
      survey_year = '2000',
      survey_mean = 100,
      proxy = list(value0 = 1, req_value = 2)
      ))
  expect_error(
    check_inputs_predict_request_year_mean(
      survey_year = 2000,
      survey_mean = '100',
      proxy = list(value0 = 1, req_value = 2)
    ))
  expect_error(
    check_inputs_predict_request_year_mean(
      survey_year = 2000,
      survey_mean = 100,
      proxy = list(value0 = '1', req_value = 2)
    ))
  expect_error(
    check_inputs_predict_request_year_mean(
      survey_year = 2000,
      survey_mean = 100,
      proxy = list(value0 = 1, value1 = '2', req_value = 2)
    ))
  expect_error(
    check_inputs_predict_request_year_mean(
      survey_year = 2000,
      survey_mean = 100,
      proxy = list(value0 = 1, req_value = '2')
    ))
  # Incorrect proxy values when using decimal year
  expect_error(
    check_inputs_predict_request_year_mean(
      survey_year = 2000.5,
      survey_mean = 100,
      proxy = list(value0 = 1, req_value = 2)
    ))
  expect_error(
    check_inputs_predict_request_year_mean(
      survey_year = c(2000, 2000.5),
      survey_mean = c(100, 101),
      proxy = list(value0 = 1,  value1 = 1, req_value = 2)
    ))
  # Non compatible lengths for survey_year and survey_mean
  expect_error(
    check_inputs_predict_request_year_mean(
      survey_year = c(2000, 2001),
      survey_mean = 100,
      proxy = list(value0 = 1, value1 = 1, req_value = 2)
    ))
  # More than two survey years
  expect_error(
    check_inputs_predict_request_year_mean(
      survey_year = c(2000, 2001, 2003),
      survey_mean = 100,
      proxy = list(value0 = 1, value1 = 1, req_value = 2)
    ))
  # More than two survey means
  expect_error(
    check_inputs_predict_request_year_mean(
      survey_year = c(2000, 2001),
      survey_mean = c(100, 102, 103),
      proxy = list(value0 = 1, value1 = 1, req_value = 2)
    ))
  # Two survey means, but only one proxy value
  expect_error(
    check_inputs_predict_request_year_mean(
      survey_year = c(2000, 2001),
      survey_mean = c(100, 105),
      proxy = list(value0 = 1, req_value = 2)
    ))
})

# compute_predicted_mean
test_that('compute_predicted_mean() returns NA if no matching method is found', {
  expect_identical(
    compute_predicted_mean(
      survey_mean = 100, proxy = list(value0 = 990, req_value = NA)),
      NA)
  expect_identical(
    compute_predicted_mean(
      survey_mean = 100, proxy = list(value0 = NA, req_value = 1000)),
    NA)
  expect_identical(
    compute_predicted_mean(
      survey_mean = c(100, 105), proxy = list(value0 = 990, value1 = NA, req_value = 1000)),
    NA)
  expect_identical(
    compute_predicted_mean(
      survey_mean = c(100, 105), proxy = list(value0 = 990, value1 = 995, req_value = NA)),
    NA)
  expect_identical(
    compute_predicted_mean(
      survey_mean = c(100, 105), proxy = list(value0 = NA, value1 = 995, req_value = 1000)),
    NA)
  expect_identical(
    compute_predicted_mean(
      survey_mean = NA, proxy = list(value0 = 990, req_value = 1000)),
    NA)
  expect_identical(
    compute_predicted_mean(
      survey_mean = c(100, NA), proxy = list(value0 = 990, value1 = 995, req_value = 1000)),
    NA)
})

# is_monotonic
test_that('is_monotonic() is working as expected', {
  expect_true(is_monotonic(x1 = 100, x2 = 110, r = 105))
  expect_false(is_monotonic(x1 = 100, x2 = 110, r = 110))
})

# is_same_direction
test_that('is_same_direction() is working as expected', {
  expect_true(is_same_direction(x = c(1, 2), y = c(2,5)))
  expect_false(is_same_direction(x = c(2, 1), y = c(2,5)))
})

# is_one_point_adjusted
test_that('is_same_direction() is working as expected', {
  expect_true(
    is_one_point_adjusted(
      survey_mean = 100,
      proxy_value = 990,
      req_value = 1000))
  expect_false(
    is_one_point_adjusted(
      survey_mean = c(100, 105),
      proxy_value = 990,
      req_value = 1000))
  expect_false(
    is_one_point_adjusted(
      survey_mean = 100,
      proxy_value = NA,
      req_value = 1000))
  expect_false(
    is_one_point_adjusted(
      survey_mean = 100,
      proxy_value = 990,
      req_value = NA))
})

# is_non_monotonic
test_that('is_non_monotonic() is working as expected', {
  expect_true(
    is_non_monotonic(
      survey_mean = c(100, 105),
      proxy_value = c(990, 1000),
      req_value = 1005))
  expect_true(
    is_non_monotonic(
      survey_mean = c(105, 100),
      proxy_value = c(990, 1000),
      req_value = 995))
  expect_false(
    is_non_monotonic(
      survey_mean = c(100, 105),
      proxy_value = c(990, 1000),
      req_value = 995))
  expect_false(
    is_non_monotonic(
      survey_mean = 100,
      proxy_value = c(990, 1000),
      req_value = 995))
  expect_false(
    is_non_monotonic(
      survey_mean = c(100, 105),
      proxy_value = c(990, 1000),
      req_value = NA))
  expect_false(
    is_non_monotonic(
      survey_mean = c(100, 105),
      proxy_value = c(NA, 1000),
      req_value = 1005))
})

# is_same_direction_interpolated
test_that('is_same_direction_interpolated() is working as expected', {
  expect_true(
    is_same_direction_interpolated(
      survey_mean = c(100, 105),
      proxy_value = c(990, 1000),
      req_value = 995))
  expect_false(
    is_same_direction_interpolated(
      survey_mean = c(105, 100),
      proxy_value = c(990, 1000),
      req_value = 995))
  expect_false(
    is_same_direction_interpolated(
      survey_mean = 105,
      proxy_value = c(990, 1000),
      req_value = 995))
  expect_false(
    is_same_direction_interpolated(
      survey_mean = c(100, 105),
      proxy_value = c(NA, 1000),
      req_value = 995))
  expect_false(
    is_same_direction_interpolated(
      survey_mean = c(100, 105),
      proxy_value = c(990, 1000),
      req_value = NA))
  expect_false(
    is_same_direction_interpolated(
      survey_mean = c(100, 105),
      proxy_value = c(990, 1000),
      req_value = 1005))
})
