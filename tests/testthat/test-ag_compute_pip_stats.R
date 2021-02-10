ag_ex1 <- readRDS('../testdata/ag_ex1.rds')
default_ppp <- list(rural = 3.039222, urban = 3.905400)
area_pop    <- list(rural = 793935216, urban = 199949784 )
requested_mean <- list(rural = 29.57366 * 12 / 365, urban = 56.34465 * 12 / 365)

test_that("multiplication works", {
  out <- ag_compute_pip_stats(welfare = ag_ex1$welfare,
                              povline = 1.9,
                              population = ag_ex1$weight,
                              area = ag_ex1$area,
                              area_pop = area_pop,
                              requested_mean = requested_mean,
                              default_ppp = default_ppp)
})
