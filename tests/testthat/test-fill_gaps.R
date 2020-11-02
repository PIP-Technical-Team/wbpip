data("md_ABC_2000_income")
data("md_ABC_2010_income")

test_that("fill_gaps() extrapolates correctly", {
  deciles <- c(0.01343078, 0.02616405, 0.03535142, 0.04437412, 0.05416520,
               0.06553887, 0.08320632, 0.10988798, 0.15703608, 0.41084518)

  res <- fill_gaps(
    request_year = 2005,
    survey_year  = 2000,
    data = list(df0 = md_ABC_2000_income),
    proxy = list(value0 = 1600, req_value = 1650),
    data_type = 'microdata',
    poverty_line = 1.9
  )

  expect_equal(res$poverty_line, 1.9)
  expect_equal(res$mean, 3543526.299, tolerance = 1.5e-5)
  expect_equal(res$median, 2093085.328, tolerance = 1.5e-5)
  expect_equal(res$headcount, 0.0001195433, tolerance = 1.5e-7)
  expect_equal(res$poverty_gap, 0.0001195433, tolerance = 1.5e-7)
  expect_equal(res$poverty_severity, 0.0001195433, tolerance = 1.5e-7)
  expect_equal(res$watts, 0)
  expect_equal(res$gini, 0.5147999, tolerance = 1.5e-7)
  expect_equal(res$mld, 0.473784,  tolerance = 1.5e-7)
  expect_equal(res$polarization, NA)
  expect_equal(res$deciles, deciles)
})
