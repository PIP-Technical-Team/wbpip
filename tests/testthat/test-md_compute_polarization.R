# Constants
dl <- readRDS(test_path("testdata", "synthetic-microdata.RDS"))
v <- c(0.4667890, 0.2457807, 0.3102726, 0.2928000, 0.4392192)
benchmark_old <- c(0.46977696, # old values from polarization function
                   0.24659578, #    using dl, this was previously skipped
                   0.31082372, #    but has still been shown in PIP
                   0.29347875,
                   0.44266146)
data("md_ABC_2000_income")
md_ABC_2000_income <-
  md_clean_data(md_ABC_2000_income,
                welfare = "welfare",
                weight  = "weight",
                quiet   = TRUE)$data

#------------------------------------------------------------------------
# Tests
#------------------------------------------------------------------------

test_that("md_compute_polarization() computations are correct", {

  # create benchmark
  bm <- vapply(dl, FUN.VALUE = numeric(1), function(x) {
    # Clean data
    df <- md_clean_data(x$data,
                        welfare = "welfare",
                        weight = "weight",
                        quiet = TRUE
    )$data
    # Order by decreasing welfare
    df <- df[order(df$welfare), ]
    # Calculate Gini
    gini   <- md_compute_gini(welfare = df$welfare,
                              weight  = df$weight) # 0.333
    med    <- fmedian(x = df$welfare,
                      w = df$weight)
    mu     <- fmean(x = df$welfare,
                    w = df$weight)
    muL    <- fmean(x = df$welfare[df$welfare < med],
                    w = df$weight[df$welfare < med])
    mustar <- mu*(1 - gini)
    benchmark <- 2*(mustar - muL)/med
    return(benchmark)
  })

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

  expect_equal(res, bm)

})


test_that("new benchmark for polarization is correct", {

  # with made up data
  welf_new   <- 1:1000
  weight_new <- rep(1, 1000)
  gini_new   <- md_compute_gini(welfare = welf_new,
                                weight  = weight_new) # 0.333
  med_new    <- fmedian(x = welf_new,
                        w = weight_new)
  mu_new     <- fmean(x = welf_new,
                      w = weight_new)
  muL_new    <- fmean(x = welf_new[welf_new < med_new],
                      w = weight_new[welf_new < med_new])
  mustar_new <- mu_new*(1 - gini_new)

  res <- md_compute_polarization(welfare = welf_new,
                                 weight  = weight_new,
                                 gini    = gini_new,
                                 mean    = mu_new,
                                 median  = med_new)

  benchmark_new <- 2*(mustar_new - muL_new)/med_new

  expect_equal(res, benchmark_new, tolerance = 1.5e-7)

  # with package data
  gini_new   <- md_compute_gini(welfare = md_ABC_2000_income$welfare,
                                weight  = md_ABC_2000_income$weight) # 0.333
  med_new    <- fmedian(x = md_ABC_2000_income$welfare,
                        w = md_ABC_2000_income$weight)
  mu_new     <- fmean(x = md_ABC_2000_income$welfare,
                      w = md_ABC_2000_income$weight)
  muL_new    <- fmean(x = md_ABC_2000_income$welfare[md_ABC_2000_income$welfare < med_new],
                      w = md_ABC_2000_income$weight[md_ABC_2000_income$welfare < med_new])
  mustar_new <- mu_new*(1 - gini_new)

  res <- md_compute_polarization(welfare = md_ABC_2000_income$welfare,
                                 weight  = md_ABC_2000_income$weight,
                                 gini    = gini_new,
                                 mean    = mu_new,
                                 median  = med_new)

  benchmark_new <- 2*(mustar_new - muL_new)/med_new

  expect_equal(res, benchmark_new)

})


