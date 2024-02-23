# Constants
dl <- readRDS(test_path("testdata", "synthetic-microdata.RDS"))
v <- c(0.4667890, 0.2457807, 0.3102726, 0.2928000, 0.4392192)

# Tests
test_that("md_compute_polarization() computations are correct", {
  # Test against synthetic microdata
  res <- vapply(dl, FUN.VALUE = numeric(1), function(x) {
    # Clean data
    df <- md_clean_data(x$data,
      welfare = "welfare",
      weight = "weight",
      quiet = TRUE
    )$data
    # Order by decreasing welfare
    df <- df[order(df$welfare), ]
    # Calculate Gini
    gini <- md_compute_gini(welfare = df$welfare, weight = df$weight)
    # Calculate Lorenz
    lz <- md_compute_lorenz(welfare = df$welfare, weight = df$weight)
    # Calculate weighted mean
    weighted_mean <- stats::weighted.mean(df$welfare, df$weight)
    # Calculate weighted median
    weighted_median <- md_compute_median(
      welfare = df$welfare,
      weight = df$weight
    )
    # Calculate polarization
    pol <- md_compute_polarization(
      welfare = df$welfare,
      weight = df$weight,
      gini = gini,
      mean = weighted_mean,
      median = weighted_median
    )
    return(pol)
  })
  skip("Watts Index computation refactoring")
  expect_equal(res, v, tolerance = 1.5e-7)
})
