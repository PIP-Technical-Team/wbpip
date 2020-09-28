# Read in synthetic microdata
dl <- readRDS('../testdata/synthetic-microdata.RDS')

# md_compute_gini
test_that('md_compute_gini() works as expected', {

  # Test handling of non-supplied weights
  expect_equal(
    md_compute_gini(welfare = 1:100),
    md_compute_gini(welfare = 1:100, weight = rep(1, 100))
  )

  # Test handling of missing values
  expect_equal(
    md_compute_gini(welfare = c(1, NA, 3, NA, 5), weight = c(10, 11, NA, 13, 14)),
    md_compute_gini(welfare = c(1, 5), weight = c(10, 14))
  )

  # Test handling of negative values
  expect_equal(
    md_compute_gini(welfare = c(1, -2, 3, -4, 5), weight = c(10, 11, -12, 13, 14)),
    md_compute_gini(welfare = c(1, 5), weight = c(10, 14))
  )

  # Test handling of unsorted welfare values
  expect_equal(
    md_compute_gini(welfare = 5:1, weight = 10:14),
    md_compute_gini(welfare = 1:5, weight = 14:10)
  )
})

test_that('md_compute_gini() computations are correct', {

  # Test perfect equality
  expect_equal(
    md_compute_gini(welfare = rep(10, 100)),
    0
  )

  # Test perfect inequality
  expect_equal(
    md_compute_gini(welfare = c(rep(0, 99), 100)),
    1
  )

  # Test against pre-computed correct values
  lapply(dl, function(x){
    df <- x$data
    res <- md_compute_gini(welfare = df$welfare, weight = df$weight)
    expect_equal(res, x$stats$gini)
  })
})

# clean_inputs_md_compute_gini
test_that('clean_inputs_md_compute_gini() works as expected', {
  expect_message(
    clean_inputs_md_compute_gini(welfare = c(1:4, NA), weight = rep(1, 5)),
    'Info: Found 1 missing value in `welfare`. This observation was removed.'
  )
  expect_message(
    clean_inputs_md_compute_gini(welfare = c(1:3, NA, NA), weight = rep(1, 5)),
    'Info: Found 2 missing values in `welfare`. These observations were removed.'
  )
  expect_message(
    clean_inputs_md_compute_gini(welfare = 1:5, weight = c(1:4, NA)),
    'Info: Found 1 missing value in `weight`. This observation was removed.'
  )
  expect_message(
    clean_inputs_md_compute_gini(welfare = 1:5 , weight = c(1:3, NA, NA)),
    'Info: Found 2 missing values in `weight`. These observations were removed.'
  )
})

# check_inputs_md_compute_gini
test_that('check_inputs_md_compute_gini() works as expected', {
  expect_error(check_inputs_md_compute_gini(welfare = 1, weight = '1'),
               paste0('`weight` must be a numeric or integer vector:\n.* ',
                      'You\'ve supplied an object of class character.'))
  expect_error(check_inputs_md_compute_gini(welfare = '1', weight = 1),
               paste0('`welfare` must be a numeric or integer vector:\n.* ',
                      'You\'ve supplied an object of class character.'))
  expect_error(check_inputs_md_compute_gini(welfare = 1, weight = list('1')),
               paste0('`weight` must be a numeric or integer vector:\n.* ',
                      'You\'ve supplied an object of class list.'))
  expect_error(check_inputs_md_compute_gini(welfare = data.frame('1'), weight = 1),
               paste0('`welfare` must be a numeric or integer vector:\n.* ',
                      'You\'ve supplied an object of class data.frame.'))
  expect_error(check_inputs_md_compute_gini(welfare = 1:2, weight = 1),
               paste0('`welfare` and `weight` must have compatible lengths:\n.* ',
               '`welfare` has length 2.\n.* `weight` has length 1.'))
})
