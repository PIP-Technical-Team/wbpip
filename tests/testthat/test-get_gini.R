# Read in synthetic microdata
dl <- readRDS('../testdata/synthetic-microdata.RDS')

test_that('get_gini() match precalculated values in synthetic-microdata.RDS', {

  # Test against pre-computed correct values
  lapply(dl, function(x){
    df <- x$data
    res <- suppressMessages(
      get_gini(df, welfare, weight, distribution_type = 'micro'))
    expect_equal(res[["gini"]], x$stats$gini)
  })
})
