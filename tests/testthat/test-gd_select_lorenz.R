# Input definition
welfare_mean    <- 51.56
ppp             <- 3.69
daily_povline   <- 1.9
monthly_povline <- daily_povline * 365 / 12

population <- c(0.0005,
                0.0032,
                0.014799999999999999,
                0.0443,
                0.0991,
                0.257,
                0.4385,
                0.5938,
                0.7089,
                1)

welfare <- c(5.824760527229386e-05,
             0.000604029410841011,
             0.0037949334793616948,
             0.013988878652244477,
             0.036992164583098786,
             0.12140708906131342,
             0.24531391873082081,
             0.37446670169288321,
             0.48753116241194566,
             1)

lq <- gd_compute_pip_stats_lq(welfare         = welfare,
                              population      = population,
                              mean            = welfare_mean,
                              povline         = monthly_povline,
                              default_ppp     = ppp)

lb <- gd_compute_pip_stats_lb(welfare         = welfare,
                              population      = population,
                              mean            = welfare_mean,
                              povline         = monthly_povline,
                              default_ppp     = ppp)

test_that('Outputs from gd_compute_pip_stats_* are consitents', {

  # Test perfect equality
  expect_equal(
    names(lq),
    names(lb)
  )
})
