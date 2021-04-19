gd_ex2 <- readRDS('../testdata/gd_ex2.RDS')

test_that("sd_create_synth_vector works as expected", {
  out <- sd_create_synth_vector(welfare = gd_ex2$welfare,
                                population = gd_ex2$weight,
                                mean = 8,
                                pop = NULL,
                                p0 = 0.5,
                                nobs = 1e5)

  expect_equal(class(out), c("data.table", "data.frame"))
  expect_equal(nrow(out), 1e5)

  out <- sd_create_synth_vector(welfare = gd_ex2$welfare,
                                population = gd_ex2$weight,
                                mean = 8,
                                pop = NULL,
                                p0 = 0.5,
                                nobs = 100)

  expect_equal(nrow(out), 100)
})

test_that("sd_create_synth_vector correctly handles population weights", {
  out <- sd_create_synth_vector(welfare = gd_ex2$welfare,
                                population = gd_ex2$weight,
                                mean = 8,
                                pop = NULL,
                                p0 = 0.5,
                                nobs = 100)

  expect_equal(unique(out$weight), 1)

  out <- sd_create_synth_vector(welfare = gd_ex2$welfare,
                                population = gd_ex2$weight,
                                mean = 8,
                                pop = 100000,
                                p0 = 0.5,
                                nobs = 100)

  expect_equal(unique(out$weight), 1000)

  out <- sd_create_synth_vector(welfare = gd_ex2$welfare,
                                population = gd_ex2$weight,
                                mean = 8,
                                pop = 500000,
                                p0 = 0.5,
                                nobs = 100)

  expect_equal(unique(out$weight), 5000)
})
