# Constants
dl <- readRDS('../testdata/synthetic-microdata.RDS')
v <- c(0.4667890, 0.2457807, 0.3102726, 0.2928000, 0.4392192)

test_that('md_compute_polarization() computations are correct', {
  # Test against synthetic microdata
  res <- vapply(dl,FUN.VALUE = numeric(1), function(x){
    df <- md_clean_data(x$data,
                        welfare = "welfare",
                        weight  = "weight")$data
    md_compute_polarization(welfare = df$welfare, weight = df$weight)
  })
  expect_equal(res, v, tolerance = 1.5e-7)
})
