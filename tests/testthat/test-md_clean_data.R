data('md_GHI_2000_income')
data('md_DEF_2000_consumption')

test_that('md_clean_data() is working correctly', {

  # Test that res$data returns as a data.table
  df <- data.frame(welfare = 1:10, weight = 1:10)
  res <- md_clean_data(
    df, welfare = 'welfare',
    weight = 'weight', quiet = TRUE)
  expect_identical(class(res$data), c('data.table', 'data.frame'))

  # Test that welfare is being sorted
  expect_true(is.unsorted(md_GHI_2000_income))
  res <- md_clean_data(
    md_GHI_2000_income, welfare = 'welfare',
    weight = 'weight', quiet = TRUE )
  expect_false(is.unsorted(res$data$welfare))

  # Test that negative welfare values are removed
  expect_true(any(sign(md_GHI_2000_income$welfare) == -1))
  res <- md_clean_data(
    md_GHI_2000_income, welfare = 'welfare',
    weight = 'weight', quiet = TRUE)
  expect_false(any(sign(res$data$welfare) == -1))
  expect_equal(res$nng_welfare, 2)

  # Test that negative weight values are removed
  df <- data.frame(welfare = 1:10, weight = c(1:9, -1))
  res <- md_clean_data(
    df, welfare = 'welfare',
    weight = 'weight',
    quiet = TRUE)
  expect_false(anyNA(res$data$weight))
  expect_equal(res$nng_weight, 1)

  # Test that missing welfare values are removed
  expect_true(anyNA(md_DEF_2000_consumption$welfare))
  res <- md_clean_data(
    md_DEF_2000_consumption, welfare = 'welfare',
    weight = 'weight', quiet = TRUE)
  expect_false(anyNA(res$data$welfare))
  expect_equal(res$nna_welfare, 3)

  # Test that missing weight values are removed
  df <- data.frame(welfare = 1:10, weight = c(1:9, NA))
  res <- md_clean_data(
    df, welfare = 'welfare',
    weight = 'weight',
    quiet = TRUE)
  expect_false(anyNA(res$data$weight))
  expect_equal(res$nna_weight, 1)

  # Test that combination of negatives
  # and zeros are removed
  df <- data.frame(
    welfare = c(1:5, NA, -1, 8:10),
    weight = c(NA, 2:7, -1, 9:10))
  res <- md_clean_data(
    df, welfare = 'welfare',
    weight = 'weight', quiet = TRUE)
  expect_equal(res$data$welfare, c(2:5, 9:10))
  expect_equal(res$data$weight, c(2:5, 9:10))

  # Test weight equal to 1 is created
  # if weight option is not provided
  df <- data.frame(welfare = 1:10)
  res <- md_clean_data(df, welfare = 'welfare', quiet = TRUE)
  expect_equal(res$data$weight, rep(1, 10))

  # Test that output messages are
  # suppressed with silent = TRUE
  df <- data.frame(
    welfare = c(1:5, NA, -1, 8:10),
    weight = c(NA, 2:7, -1, 9:10))
  expect_silent(md_clean_data(
    df, welfare = 'welfare',
    weight = 'weight',
    quiet = TRUE))

})

