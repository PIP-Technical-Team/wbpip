test_that('md_clean_inputs() works as expected', {
  expect_message(
    md_clean_inputs(welfare = c(1:4, NA), weight = rep(1, 5)),
    'Info: Found 1 missing value in `welfare`. This observation was removed.'
  )
  expect_message(
    md_clean_inputs(welfare = c(1:3, NA, NA), weight = rep(1, 5)),
    'Info: Found 2 missing values in `welfare`. These observations were removed.'
  )
  expect_message(
    md_clean_inputs(welfare = 1:5, weight = c(1:4, NA)),
    'Info: Found 1 missing value in `weight`. This observation was removed.'
  )
  expect_message(
    md_clean_inputs(welfare = 1:5 , weight = c(1:3, NA, NA)),
    'Info: Found 2 missing values in `weight`. These observations were removed.'
  )
})

