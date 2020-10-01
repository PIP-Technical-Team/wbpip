test_that('md_check_inputs() works as expected', {
  expect_error(md_check_inputs(welfare = 1, weight = '1'),
               paste0('`weight` must be a numeric or integer vector:\n.* ',
                      'You\'ve supplied an object of class character.'))
  expect_error(md_check_inputs(welfare = '1', weight = 1),
               paste0('`welfare` must be a numeric or integer vector:\n.* ',
                      'You\'ve supplied an object of class character.'))
  expect_error(md_check_inputs(welfare = 1, weight = list('1')),
               paste0('`weight` must be a numeric or integer vector:\n.* ',
                      'You\'ve supplied an object of class list.'))
  expect_error(md_check_inputs(welfare = data.frame('1'), weight = 1),
               paste0('`welfare` must be a numeric or integer vector:\n.* ',
                      'You\'ve supplied an object of class data.frame.'))
  expect_error(md_check_inputs(welfare = 1:2, weight = 1),
               paste0('`welfare` and `weight` must have compatible lengths:\n.* ',
                      '`welfare` has length 2.\n.* `weight` has length 1.'))
})
