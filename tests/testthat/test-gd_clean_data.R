# Recreate data from DAtt 1998 and Group 5 in povcalnet website.

  gd <- tibble::tribble(
    ~W,    ~X,      ~P,      ~L,
    0.92,   24.84,  0.0092, 0.00208,
    2.47,    35.8,  0.0339, 0.01013,
    5.11,   45.36,   0.085, 0.03122,
    7.9,    55.1,   0.164, 0.07083,
    9.69,   64.92,  0.2609, 0.12808,
    15.24,   77.08,  0.4133, 0.23498,
    13.64,   91.75,  0.5497, 0.34887,
    16.99,  110.64,  0.7196, 0.51994,
    10,   134.9,  0.8196,  0.6427,
    9.78,  167.76,  0.9174, 0.79201,
    3.96,  215.48,   0.957, 0.86966,
    1.81,  261.66,  0.9751, 0.91277,
    2.49,  384.97,       1,       1
  )

  setDT(gd)

  gd[, R := (W*X)/sum(W*X)]

test_that("making sure standardization works", {

  dt1 <- gd_clean_data(gd,
                       welfare    = L,
                       population = P,
                       data_type  = 1)

  dt2 <- gd_clean_data(gd,
                       welfare    = R,
                       population = W,
                       data_type  = 2)

  dt5 <- gd_clean_data(gd,
                       welfare    = X,
                       population = W,
                       data_type  = 5)

  colnames <- c("population", "welfare")
  dt2a <- dt2[, lapply(.SD, round, digits = 4),
              .SDcols = colnames]

  dt5a <- dt5[, lapply(.SD, round, digits = 4),
              .SDcols = colnames]

  dt1a <- dt1[, lapply(.SD, round, digits = 4),
              .SDcols = colnames]


  expect_equal(dt2, dt5)
  expect_equal(dt1a, dt2a)
  expect_equal(dt1a, dt5a)

})
