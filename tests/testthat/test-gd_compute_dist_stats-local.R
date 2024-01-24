skip_if(Sys.getenv('WBPIP_RUN_LOCAL_TESTS') != "TRUE")

idn1990 <- readRDS(test_path("testdata", "local/idn1990.RDS"))
zwe2011 <- readRDS(test_path("testdata", "local/zwe2011.RDS"))

test_that("gd_compute_dist_stats() returns correct results for production data", {

  # IDN 1990 Urban
  df <- idn1990[idn1990$area == "urban", ]
  df <- gd_clean_data(df,
    welfare = "welfare",
    population = "weight",
    gd_type = 5,
    quiet = TRUE
  )
  res <- gd_compute_dist_stats(
    welfare = df$welfare, population = df$weight,
    mean = 88.56682
  )
  expect_equal(res$mean, 88.56682)
  expect_equal(res$median, 69.67302, tolerance = .5e-06)
  expect_equal(res$gini, 0.3466288, tolerance = .5e-06)
  expect_equal(res$mld, 0.1965911, tolerance = .5e-06)
  expect_equal(res$polarization, 0.2920545, tolerance = .5e-07)
  expect_equal(res$deciles,
    tolerance = 7e-05,
    c(
      0.03462, 0.04439, 0.05376, 0.06321, 0.07327,
      0.08465, 0.09856, 0.1176, 0.1499, 0.28
    )
  )

  # IDN 1990 Rural
  df <- idn1990[idn1990$area == "rural", ]
  df <- gd_clean_data(df,
    welfare = "welfare",
    population = "weight",
    gd_type = 5,
    quiet = TRUE
  )
  res <- gd_compute_dist_stats(
    welfare = df$welfare, population = df$weight,
    mean = 57.83237
  )
  expect_equal(res$mean, 57.83237)
  expect_equal(res$median, 50.26953, tolerance = .5e-06)
  expect_equal(res$gini, 0.2645508, tolerance = .5e-06)
  expect_equal(res$mld, 0.1143787, tolerance = .5e-06)
  expect_equal(res$polarization, 0.2136762, tolerance = .5e-06)
  expect_equal(res$deciles,
    tolerance = 1.1e-04,
    c(
      0.04447, 0.05561, 0.06503, 0.07375, 0.08243,
      0.09176, 0.1027, 0.1171, 0.1408, 0.2263
    )
  )

  # ZWE 2011 National
  skip("gd_compute_pip_stats() not working for this example")
  df <- zwe2011
  mean <- stats::weighted.mean(df$welfare, w = df$weight) #* 365/12
  df <- gd_clean_data(df,
    welfare = "welfare",
    population = "weight",
    gd_type = 5,
    quiet = TRUE
  )
  res1 <- gd_compute_pip_stats(
    welfare = df$welfare, population = df$weight,
    povline = 1.9, mean = mean
  )
  res2 <- gd_compute_dist_stats(
    welfare = df$welfare, population = df$weight,
    mean = mean
  )
  expect_equal(res1$mean, res2$mean)
  expect_equal(res1$median, res2$median)
  expect_equal(res1$gini, res2$gini)
  expect_equal(res1$mld, res2$mld)
  expect_equal(res1$polarization, res2$polarization)
  expect_equal(res1$deciles, res2$deciles)
})
