# Read in synthetic microdata
dl <- readRDS('../testdata/synthetic-microdata.RDS')

test_that('get_gini() works as expected', {

  expect_equal(2+2, 4)
})

context("get_gini() match precalculated values in synthetic-microdata.RDS")
test_that('get_gini() computations are correct', {

  # Test against pre-computed correct values
  lapply(dl, function(x){
    df <- x$data
    res <- get_gini(df, welfare, weight)
    expect_equal(res[["gini"]], x$stats$gini)
  })
})
