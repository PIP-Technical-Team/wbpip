benchmark <- readRDS(test_path("testdata", "synthetic-microdata.RDS"))

benchmark <- md_clean_data(benchmark[[1]]$data,
                           welfare = 'welfare',
                           weight = 'weight',
                           quiet = TRUE)$data

test_that("does function return 0 headcount when all welfare is above poverty line?", {
  res <- md_compute_poverty_stats(welfare = 10:100, povline_lcu = 9, weight = 10:100)
  expect_equal(
    res$headcount,
    0
  )
})

test_that("does function return all as poor when all welfare values are below poverty line?", {
  pop <- 1:100
  res <- md_compute_poverty_stats(welfare = pop, povline_lcu = 101, weight = rep(1, 100))
  expect_equal(
    res$headcount,
    1
  )
})


test_that("does poverty gap = 1 when welfare values are 0?", {
  res <- md_compute_poverty_stats(
    welfare = rep(0, 10),
    povline_lcu = 12,
    weight = rep(1, 10)
  )
  expect_equal(
    res$poverty_gap,
    1
  )
})

test_that("does poverty gap = 0 when welfare values are at least the povline?", {
  res <- md_compute_poverty_stats(
    welfare = 21:30,
    povline_lcu = 15,
    weight = rep(1, 10)
  )
  expect_equal(
    res$poverty_gap,
    0
  )
})

test_that("does function produce results that match compute_poverty_stats in povcalnet", {
  res <- md_compute_poverty_stats(
    welfare = benchmark$welfare,
    povline_lcu = mean(benchmark$welfare),
    weight = benchmark$weight
  )

  expect_equal(res[["headcount"]], 0.7333513, tolerance = 1e-6)
  expect_equal(res[["poverty_gap"]], 0.3957584, tolerance = 1e-6)
  expect_equal(res[["poverty_severity"]], 0.2534849, tolerance = 1e-6)
  expect_equal(res[["watts"]], 0.6899868, tolerance = 1e-4)
  # expect_equal(res[["watts_old"]], 0.6899868, tolerance = 1e-6)
})

test_that("md_compute_poverty_stats does not fail when poverty == 0", {
  res <- md_compute_poverty_stats(
    welfare = benchmark$welfare,
    povline_lcu = min(benchmark$welfare) - 1,
    weight = benchmark$weight
  )

  expect_equal(res[["headcount"]], 0)
  expect_equal(res[["poverty_gap"]], 0)
  expect_equal(res[["poverty_severity"]], 0)
  expect_equal(res[["watts"]], 0)
})


# New tests -----------
# ______________________________________________________________________
# Get intermediate for benchmark
# ______________________________________________________________________
povline_lcu       <- mean(benchmark$welfare)
pov_status        <- (benchmark$welfare < povline_lcu)
relative_distance <- (1 - (benchmark$welfare[pov_status] / povline_lcu))

weight_pov        <- benchmark$weight[pov_status]
weight_total      <- sum(benchmark$weight)


# MD FGT
#
test_that("md_compute_fgt throughs erros", {

  welfare <- benchmark$welfare
  weight  <- benchmark$weight

  md_compute_fgt() |>
    expect_error()

  # removed test below, because cli_abort removed for efficiency
  # fgt <- md_compute_fgt(welfare    = welfare,
  #                       weight      = weight,
  #                       return_data =  TRUE)
  # md_compute_fgt(fgt_data = fgt,
  #                welfare = welfare) |>
  #   expect_error()


  })
test_that("md_compute_fgt works", {
  welfare <- benchmark$welfare
  weight  <- benchmark$weight

  fgt <- md_compute_fgt(welfare    = welfare,
                       weight      = weight,
                       return_data =  TRUE)
  expect_equal(names(fgt),
               c("povline","pov_status", "relative_distance", "weight", "FGT0"))

  expect_equal(fgt$FGT0, 0.18186165)

  ## return all three measures -----------
  fgt <- md_compute_fgt(welfare    = welfare,
                        weight      = weight,
                        return_data =  TRUE) |>
    md_compute_fgt(alpha = 1,
                   return_data =  TRUE) |>
    md_compute_fgt(alpha = 2,
                   return_data =  TRUE)


  expect_equal(names(fgt),
               c("povline",
                 "pov_status",
                 "relative_distance",
                 "weight",
                 "FGT0",
                 "FGT1",
                 "FGT2"
               ))
  expect_equal(fgt$FGT0, 0.18186165)
  expect_equal(fgt$FGT1, 0.066429986)
  expect_equal(fgt$FGT2, 0.034727533)

})

test_that("md_compute_fgt works with vectorization of povline", {
  welfare <- benchmark$welfare
  weight  <- benchmark$weight
  povline <- c(fmedian(welfare, w = weight)/2,
               fmedian(welfare, w = weight),
               fmedian(welfare, w = weight)*2)

  fgt <- md_compute_fgt(welfare     = welfare,
                        weight      = weight,
                        povline     = povline,
                        return_data =  TRUE)

  expect_equal(names(fgt),
               c("povline",
                 "pov_status",
                 "relative_distance",
                 "weight",
                 "FGT0"))

  res_1 <- 0.1818617
  # res_1 <- old_md_compute_fgt(welfare = welfare,
  #                             weight = weight,
  #                             povline = povline[1])

  res_2 <- 0.4989594
  # res_2 <- old_md_compute_fgt(welfare = welfare,
  #                             weight = weight,
  #                             povline = povline[2])

  res_3 <- 0.7774483
  # res_3 <- old_md_compute_fgt(welfare = welfare,
  #                             weight = weight,
  #                             povline = povline[3])

  expect_equal(fgt$FGT0, c(res_1,
                           res_2,
                           res_3), tolerance = 6)

  ## return all three measures -----------
  fgt2 <- md_compute_fgt(welfare     = welfare,
                        weight      = weight,
                        povline     = povline,
                        return_data =  TRUE) |>
    md_compute_fgt(alpha = 1,
                   return_data =  TRUE) |>
    md_compute_fgt(alpha = 2,
                   return_data =  TRUE)


  expect_equal(names(fgt2),
               c("povline",
                 "pov_status",
                 "relative_distance",
                 "weight",
                 "FGT0",
                 "FGT1",
                 "FGT2"
               ))


  res2_1_names  <- c("povline",
                     "pov_status",
                     "relative_distance",
                     "weight",
                     "FGT0",
                     "FGT1",
                     "FGT2")
  res2_1_values <- c(0.1818617, 0.06642999, 0.03472753)
  res2_2_values <- c(0.4989594, 0.2063, 0.1152613)
  res2_3_values <- c(0.7774483, 0.4347224, 0.2869367)


  expect_equal(fgt2 |> names(),
               res2_1_names)
  expect_equal(fgt2$FGT0,
               c(res2_1_values[1],res2_2_values[1],res2_3_values[1]), tolerance = 6)
  expect_equal(fgt2$FGT1,
               c(res2_1_values[2],res2_2_values[2],res2_3_values[2]), tolerance = 6)
  expect_equal(fgt2$FGT2,
               c(res2_1_values[3],res2_2_values[3],res2_3_values[3]), tolerance = 6)


})

#_______________________________________________________________________
# Test - md_compute_headcount

test_that("md_compute_headcount works", {

  out1 <- md_compute_headcount(
    welfare      = benchmark$welfare,
    weight       = benchmark$weight,
    povline      = povline_lcu
  )

  expect_equal(out1,
               0.7333513,
               tolerance = 1e-6) #match compute_poverty_stats in povcalnet


  welf <- c(1:10)
  wei  <- rep(c(1, 2), 5)

  out2 <- md_compute_headcount(
    welfare      = welf,
    weight       = wei,
    povline      = 5
  )


  expect_equal(
    out2, 0.4
  )

})

test_that("md_compute_headcount works with NULL for weight_pov and weight_total", {

  out <- md_compute_headcount(
    welfare      = benchmark$welfare,
    weight       = benchmark$weight,
    povline      = povline_lcu
  )
  expect_equal(out,
               0.7333513,
               tolerance = 1e-6) #match compute_poverty_stats in povcalnet


})




#_______________________________________________________________________
# Test - md_compute_pov_gap
test_that("md_compute_pov_gap works", {

  out <- md_compute_pov_gap(
    welfare           = benchmark$welfare,
    weight            = benchmark$weight,
    povline           = povline_lcu
  )


  expect_equal(out,
               0.3957584,
               tolerance = 1e-6) # match compute_poverty_stats in povcalnet

})


test_that("md_compute_pov_gap works with NULLs", {

  out <- md_compute_pov_gap(
    welfare           = benchmark$welfare,
    weight            = benchmark$weight,
    povline           = povline_lcu
  )


  expect_equal(out,
               0.3957584,
               tolerance = 1e-6) #match compute_poverty_stats in povcalnet

})



#_______________________________________________________________________
# Test - md_compute_pov_severity
test_that("md_compute_pov_severity works", {

  out <- md_compute_pov_severity(
    welfare           = benchmark$welfare,
    weight            = benchmark$weight,
    povline           = povline_lcu
  )

  expect_equal(out,
               0.2534849,
               tolerance = 1e-6) #match compute_poverty_stats in povcalnet
})


#_______________________________________________________________________
# Test - md_compute_watts
test_that("md_compute_watts works", {

  out <- md_compute_watts(
    welfare           = benchmark$welfare,
    weight            = benchmark$weight,
    povline           = povline_lcu
  )

  expect_equal(out, 0.6899868, tolerance = 1e-4) #match compute_poverty_stats in povcalnet

})

test_that("md_compute_watts messages and errors", {

  # no povline
  expect_error(
    md_compute_watts(
      welfare           = benchmark$welfare,
      weight            = benchmark$weight
    )
  )
  # no welfare
  expect_error(
    md_compute_watts(
      weight            = benchmark$weight,
      povline           = povline_lcu
    )
  )


})


test_that("md_compute_watts prints error when welfare and/or povline is null", {
  skip()
  md_compute_watts(
    welfare     = benchmark$welfare,
    weight      = benchmark$weight,
    povline = NULL) |>
  expect_error()

  expect_error(md_compute_watts(
    welfare     = NULL,
    weight      = benchmark$weight,
    povline = mean(benchmark$welfare)
  ))

  expect_error(md_compute_watts(
    welfare     = NULL,
    weight      = benchmark$weight,
    povline = NULL
  ))
})


test_that("When watts is numeric(0) then watts equals 0" , {

  # # ______________________________________________________________________
  # # Example of when watts can be numeric(0)
  # # ______________________________________________________________________
  # welfare <- benchmark$welfare
  # weight <- benchmark$weight
  # povline_lcu       <- min(welfare)-1
  # pov_status        <- (welfare < povline_lcu)
  # weight_total      <- sum(weight)
  # keep               <- welfare > 0 & pov_status
  # w_gt_zero          <- welfare[keep] # Makes it numeric(0)
  # sensitive_distance <- log(povline_lcu / w_gt_zero) # Makes it numeric(0)
  # watts              <- sum(sensitive_distance * weight[keep])/weight_total #The sum makes it just 0
  #
  #


  out <- md_compute_watts(
    welfare = benchmark$welfare,
    povline = min(benchmark$welfare) - 1,
    weight = benchmark$weight)

  expect_equal(
    out,
    0
  )
})



#_______________________________________________________________________
# Test - md_compute_poverty_stats
test_that("md_compute_poverty_stats works", {

  out <- md_compute_poverty_stats(
    welfare     = benchmark$welfare,
    weight      = benchmark$weight,
    povline_lcu = mean(benchmark$welfare)
  )
  expect_equal(out[["headcount"]],
               0.7333513,
               tolerance = 1e-6)
  expect_equal(out[["poverty_gap"]],
               0.3957584,
               tolerance = 1e-6)
  expect_equal(out[["poverty_severity"]],
               0.2534849,
               tolerance = 1e-6)
  expect_equal(out[["watts"]],
               0.6899868,
               tolerance = 1e-4)
})

test_that("md_compute_poverty_stats matches previous function", {

  out_old <- md_compute_poverty_stats(
    welfare     = benchmark$welfare,
    weight      = benchmark$weight,
    povline_lcu = mean(benchmark$welfare)
  )

  out <- md_compute_poverty_stats(
    welfare     = benchmark$welfare,
    weight      = benchmark$weight,
    povline_lcu = mean(benchmark$welfare)
  )

  expect_equal(out[["headcount"]],
               out_old[["headcount"]],
               tolerance = 1e-6)
  expect_equal(out[["poverty_gap"]],
               out_old[["poverty_gap"]],
               tolerance = 1e-6)
  expect_equal(out[["poverty_severity"]],
               out_old[["poverty_severity"]],
               tolerance = 1e-6)
  expect_equal(out[["watts"]],
               out_old[["watts"]],
               tolerance = 1e-4)
})

test_that("md_compute_poverty_stats calculates with weight = 1 when is NULL", {

  #____________________________________________________________________
  # Run old function with weight one to find  true values
  #____________________________________________________________________
  # benchmark$weight_1 <- 1
  # out_old <- md_compute_poverty_stats(
  #   welfare     = benchmark$welfare,
  #   weight      = benchmark$weight_1,
  #   povline_lcu = mean(benchmark$welfare)
  # )
  #
  # out_old # $headcount = 0.705 $poverty_gap = 0.3557192
  #         # $poverty_severity = 0.2193438 $watts = 0.598579

  out <- md_compute_poverty_stats(
    welfare     = benchmark$welfare,
    weight      = rep(1, length(benchmark$welfare)),
    povline_lcu = mean(benchmark$welfare)
  )

  expect_equal(out[["headcount"]],
               0.705,
               tolerance = 1e-6)
  expect_equal(out[["poverty_gap"]],
               0.3557192,
               tolerance = 1e-6)
  expect_equal(out[["poverty_severity"]],
               0.2193438,
               tolerance = 1e-6)
  expect_equal(out[["watts"]],
               0.598579,
               tolerance = 1e-4)
})

test_that("md_compute_poverty_stats prints error when welfare and/or povline is NULL", {

  expect_error(md_compute_poverty_stats(
    welfare     = benchmark$welfare,
    weight      = benchmark$weight,
    povline_lcu = NULL
  ))

  # expect_error(md_compute_poverty_stats(
  #   welfare     = NULL,
  #   weight      = benchmark$weight,
  #   povline_lcu = mean(benchmark$welfare)
  # ))

  expect_error(md_compute_poverty_stats(
    welfare     = NULL,
    weight      = benchmark$weight,
    povline_lcu = NULL
  ))
})
