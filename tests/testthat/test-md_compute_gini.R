# Read in synthetic microdata
dl <- readRDS('../testdata/synthetic-microdata.RDS')

# Data preparations
dl <- lapply(dl, function(x){
  df <- x$data
  # Order by increasing welfare values
  df <- df[order(df$welfare),]
  # Remove rows with missing welfare values
  df <- df[!is.na(df$welfare),]
  # Remove rows with negative welfare values
  df <- df[df$welfare > 0 ,]
  # Select data
  x$data <- df
  return(x)
})

# md_compute_gini
test_that('md_compute_gini() returns the correct result', {
  lapply(dl, function(x){
    df <- x$data
    res <- md_compute_gini(measure = df$welfare, weight = df$weight)
    expect_equal(res, x$stats$gini)
  })
})

# check_inputs_md_compute_gini
test_that('check_inputs_md_compute_gini() works as expected', {
  expect_error(check_inputs_md_compute_gini(measure = 1:2, weight = c(1, NA)),
               '`weight` cannot contain missing values.')
  expect_error(check_inputs_md_compute_gini(measure = c(1, NA), weight = 1:2),
               '`measure` cannot contain missing values.')
  expect_error(check_inputs_md_compute_gini(measure = 1:2, weight = c(-1, 2)),
               '`weight` cannot contain negative values.')
  expect_error(check_inputs_md_compute_gini(measure = c(-1, 1), weight = 1:2),
               '`measure` cannot contain negative values.')
  expect_error(check_inputs_md_compute_gini(measure = 1, weight = '1'),
               paste0('`weight` must be a numeric or integer vector:\n.* ',
                      'You\'ve supplied an object of class character.'))
  expect_error(check_inputs_md_compute_gini(measure = '1', weight = 1),
               paste0('`measure` must be a numeric or integer vector:\n.* ',
                      'You\'ve supplied an object of class character.'))
  expect_error(check_inputs_md_compute_gini(measure = 1, weight = list('1')),
               paste0('`weight` must be a numeric or integer vector:\n.* ',
                      'You\'ve supplied an object of class list.'))
  expect_error(check_inputs_md_compute_gini(measure = data.frame('1'), weight = 1),
               paste0('`measure` must be a numeric or integer vector:\n.* ',
                      'You\'ve supplied an object of class data.frame.'))
  expect_error(check_inputs_md_compute_gini(measure = 1:2, weight = 1),
               paste0('`measure` and `weight` must have compatible lengths:\n.* ',
               '`measure` has length 2.\n.* `weight` has length 1.'))
  expect_error(check_inputs_md_compute_gini(measure = 3:1, weight = 1:3),
               '`measure` must be sorted in increasing order.')
})
