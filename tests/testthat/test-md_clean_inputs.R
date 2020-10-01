test_that('md_clean_inputs() works as expected', {
  # Missing values
  expect_message(
    md_clean_inputs(welfare = c(1:4, NA), weight = rep(1, 5)),
    paste0('Info: Found 1 observation with missing values. This observation was removed.\n.*',
           '1 missing value in `welfare`.\n.*',
           '0 missing values in `weight`.')
  )
  expect_message(
    md_clean_inputs(welfare = c(1:4, NA), weight = c(1:4, NA)),
    paste0('Info: Found 1 observation with missing values. This observation was removed.\n.*',
           '1 missing value in `welfare`.\n.*',
           '1 missing value in `weight`.')
  )
  expect_message(
    md_clean_inputs(welfare = c(1:3, NA, NA), weight = c(NA, 1:4)),
    paste0('Info: Found 3 observations with missing values. These observations were removed.\n.*',
           '2 missing values in `welfare`.\n.*',
           '1 missing value in `weight`.')
  )
  expect_message(
    md_clean_inputs(welfare = c(NA, 1, 4, 5, NA, 1, 1), weight = c(10, NA, 12, 11, NA, 13, NA)),
    paste0('Info: Found 4 observations with missing values. These observations were removed.\n.*',
           '2 missing values in `welfare`.\n.*',
           '3 missing values in `weight`.')
  )
  # Negative values
  expect_message(
    md_clean_inputs(welfare = c(1:4, -1), weight = rep(1, 5)),
    paste0('Info: Found 1 observation with negative values. This observation was removed.\n.*',
           '1 negative value in `welfare`.\n.*',
           '0 negative values in `weight`.')
  )
  expect_message(
    md_clean_inputs(welfare = c(1:4, -1), weight = c(1:4, -1)),
    paste0('Info: Found 1 observation with negative values. This observation was removed.\n.*',
           '1 negative value in `welfare`.\n.*',
           '1 negative value in `weight`.')
  )
  expect_message(
    md_clean_inputs(welfare = c(1:3, -1, -1), weight = c(-1, 1:4)),
    paste0('Info: Found 3 observations with negative values. These observations were removed.\n.*',
           '2 negative values in `welfare`.\n.*',
           '1 negative value in `weight`.')
  )
  expect_message(
    md_clean_inputs(welfare = c(-1, 1, 4, 5, -1, 1, 1), weight = c(10, -1, 12, 11, -1, 13, -1)),
    paste0('Info: Found 4 observations with negative values. These observations were removed.\n.*',
           '2 negative values in `welfare`.\n.*',
           '3 negative values in `weight`.')
  )
})

