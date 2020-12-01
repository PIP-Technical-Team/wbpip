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
                              requested_mean  = welfare_mean,
                              povline         = monthly_povline,
                              default_ppp     = ppp)

lb <- gd_compute_pip_stats_lb(welfare         = welfare,
                              population      = population,
                              requested_mean  = welfare_mean,
                              povline         = monthly_povline,
                              default_ppp     = ppp)

test_that('Outputs from gd_compute_pip_stats_* are consitents', {

  # Test perfect equality
  expect_equal(
    names(lq),
    names(lb)
  )
})


# test gd_select_lorenz_pov() ---------------------------------------------

#-----------------------------
#  Qv  Bv  Qc  Bc pov_flag use
#-----------------------------
#  X   X   X   X   15     smaller SSEz
#  X   X   X   O   14     Q
#  X   X   O   X   13     B
#  X   X   O   O   12     Q
#-----------------------------
#  X   O   X   X   11     Q
#  X   O   X   O   10     Q
#  X   O   O   X    9     B
#  X   O   O   O    8     Q
#-----------------------------
#  O   X   X   X    7     B
#  O   X   X   O    6     Q
#  O   X   O   X    5     B
#  O   X   O   O    4     B
#-----------------------------
#  O   O   X   X    3     smaller SSEz
#  O   O   X   O    2     Q
#  O   O   O   X    1     B
#  O   O   O   O    0     Q
#-----------------------------

test_that('gd_select_lorenz_pov is correct when all fits are normal and valid', {

  # Use smaller SSEz for selection
  lq$is_valid  <- TRUE
  lq$is_normal <- TRUE
  lb$is_valid  <- TRUE
  lb$is_normal <- TRUE
  lq$ssez      <- 2
  lb$ssez      <- 1

  # LB has better fit
  # pov_flag = 15
  expect_equal(
    gd_select_lorenz_pov(lq = lq, lb = lb),
    FALSE
  )

  # LQ has better fit
  # pov_flag = 15
  lb$ssez <- lq$ssez + 1
  expect_equal(
    gd_select_lorenz_pov(lq = lq, lb = lb),
    TRUE
  )
}
)

test_that('gd_select_lorenz_pov is correct when only LQ is normal and valid', {

  # LB normal but not valid
  # pov_flag = 11
  lq$is_valid  <- TRUE
  lq$is_normal <- TRUE
  lb$is_valid  <- FALSE
  lb$is_normal <- TRUE
  expect_equal(
    gd_select_lorenz_pov(lq = lq, lb = lb),
    TRUE
  )

  # LB valid but not normal
  # pov_flag = 14
  lq$is_valid  <- TRUE
  lq$is_normal <- TRUE
  lb$is_valid  <- TRUE
  lb$is_normal <- FALSE
  expect_equal(
    gd_select_lorenz_pov(lq = lq, lb = lb),
    TRUE
  )

  # LB invalid and not normal
  # pov_flag = 10
  lq$is_valid  <- TRUE
  lq$is_normal <- TRUE
  lb$is_valid  <- FALSE
  lb$is_normal <- FALSE
  expect_equal(
    gd_select_lorenz_pov(lq = lq, lb = lb),
    TRUE
  )
}
)

test_that('gd_select_lorenz_pov is correct when only LB is normal and valid', {

  # LQ normal but not valid
  # pov_flag = 7
  lq$is_valid  <- FALSE
  lq$is_normal <- TRUE
  lb$is_valid  <- TRUE
  lb$is_normal <- TRUE
  expect_equal(
    gd_select_lorenz_pov(lq = lq, lb = lb),
    FALSE
  )

  # LQ valid but not normal
  # pov_flag = 13
  lq$is_valid  <- TRUE
  lq$is_normal <- FALSE
  lb$is_valid  <- TRUE
  lb$is_normal <- TRUE
  expect_equal(
    gd_select_lorenz_pov(lq = lq, lb = lb),
    FALSE
  )

  # LQ invalid and not normal
  # pov_flag = 5
  lq$is_valid  <- FALSE
  lq$is_normal <- FALSE
  lb$is_valid  <- TRUE
  lb$is_normal <- TRUE
  expect_equal(
    gd_select_lorenz_pov(lq = lq, lb = lb),
    FALSE
  )
}
)

test_that('gd_select_lorenz_pov is correct when both LQ and LB are invalid', {
  # Both LQ and LB are normal. Best SSEz fit
  lq$is_valid  <- FALSE
  lq$is_normal <- TRUE
  lb$is_valid  <- FALSE
  lb$is_normal <- TRUE
  lq$ssez      <- 2
  lb$ssez      <- 1

  # pov_flag = 3
  expect_equal(
    gd_select_lorenz_pov(lq = lq, lb = lb),
    FALSE
  )

  lb$ssez      <- lq$ssez + 1
  expect_equal(
    gd_select_lorenz_pov(lq = lq, lb = lb),
    TRUE
  )

  # LQ is normal
  # pov_flag = 2
  lq$is_valid  <- FALSE
  lq$is_normal <- TRUE
  lb$is_valid  <- FALSE
  lb$is_normal <- TRUE
  expect_equal(
    gd_select_lorenz_pov(lq = lq, lb = lb),
    TRUE
  )

  # LB is normal
  # pov_flag = 1
  lq$is_valid  <- FALSE
  lq$is_normal <- FALSE
  lb$is_valid  <- FALSE
  lb$is_normal <- TRUE
  expect_equal(
    gd_select_lorenz_pov(lq = lq, lb = lb),
    FALSE
  )

  # Both LQ and LB are not normal
  # pov_flag = 0
  lq$is_valid  <- FALSE
  lq$is_normal <- FALSE
  lb$is_valid  <- FALSE
  lb$is_normal <- FALSE
  expect_equal(
    gd_select_lorenz_pov(lq = lq, lb = lb),
    TRUE
  )
}
)

test_that('gd_select_lorenz_pov is correct for remaining cases', {

  # Both LQ and LB are normal and not valid
  # pov_flag = 12
  lq$is_valid  <- TRUE
  lq$is_normal <- FALSE
  lb$is_valid  <- TRUE
  lb$is_normal <- FALSE
  expect_equal(
    gd_select_lorenz_pov(lq = lq, lb = lb),
    TRUE
  )

  # LQ is valid, LB is normal
  # pov_flag = 9
  lq$is_valid  <- TRUE
  lq$is_normal <- FALSE
  lb$is_valid  <- FALSE
  lb$is_normal <- TRUE
  expect_equal(
    gd_select_lorenz_pov(lq = lq, lb = lb),
    FALSE
  )

  # LQ is valid, all rest is not
  # pov_flag = 8
  lq$is_valid  <- TRUE
  lq$is_normal <- FALSE
  lb$is_valid  <- FALSE
  lb$is_normal <- FALSE
  expect_equal(
    gd_select_lorenz_pov(lq = lq, lb = lb),
    TRUE
  )

  # LB is valid, LQ is normal
  # pov_flag = 6
  lq$is_valid  <- FALSE
  lq$is_normal <- TRUE
  lb$is_valid  <- TRUE
  lb$is_normal <- FALSE
  expect_equal(
    gd_select_lorenz_pov(lq = lq, lb = lb),
    TRUE
  )

  # LB is valid
  # pov_flag = 4
  lq$is_valid  <- FALSE
  lq$is_normal <- FALSE
  lb$is_valid  <- TRUE
  lb$is_normal <- FALSE
  expect_equal(
    gd_select_lorenz_pov(lq = lq, lb = lb),
    FALSE
  )
}
)
