test_that('deflate_welfare_mean()', {
  expect_equal(
    deflate_welfare_mean(10, 3, 2),
    1.66666667)
  expect_equal(
    deflate_welfare_mean(100, .3, 100),
    3.3333333)
  expect_equal(
    deflate_welfare_mean(122544.6, ppp = 0.677498, cpi = 209.3907),
    863.8313, tolerance = 1.5e-6)
})
