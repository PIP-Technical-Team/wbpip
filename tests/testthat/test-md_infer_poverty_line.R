
context("uniform distribution of welfare")
test_that("multiplication works", {
  set.seed(10010)
  uv   <- 5
  ps   <- runif(1)
  size <- 1001
  expect_equal(md_infer_poverty_line(rep(uv, size),
                                     popshare = ps),
               uv)

  expect_equal(md_infer_poverty_line(rep(uv, size),
                                     sample(1:500, size, replace = TRUE),
                                     popshare = ps),
               uv)

})

context("infer_poverty works in any point of the distribution")
test_that("2 digits of tolerance", {

  fgt <- function(welfare,
                  povline,
                  alpha   = 0,
                  equal   = FALSE) {

    if (equal == FALSE) {
      x <-  1*((welfare < povline)*(1 - (welfare / povline))^alpha)
    } else {
      x <-  1*((welfare <= povline)*(1 - (welfare / povline))^alpha)
    }

    return(x)
  }


  set.seed(10010)
  tl     <- 2 # tolerance
  df     <- md_ABC_2000_income
  shares <- c(0, round(runif(15), digits = tl), 1)

  res <- lapply(shares, function(x){

    i1 <- md_infer_poverty_line(df$welfare, df$weight, popshare = x)
    s1 <- fgt(df$welfar, i1, equal = TRUE)
    hc <- weighted.mean(s1, df$weight, na.rm = TRUE)
    hc <- round(hc, digits = tl)

    expect_equal(hc, x)
    # cat(paste("\nhc:", hc, "\nx :", x, "\n"))

  })

})
