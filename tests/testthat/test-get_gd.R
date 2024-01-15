welfare <-
  c(
    0.020325872283517,
    0.0486668586141716,
    0.089560329230562,
    0.134981755014038,
    0.17908510090811,
    0.232779565006409,
    0.305883167269907,
    0.386423998137644,
    0.464334770243989,
    0.531368273739212,
    0.602837584482586,
    0.679074672994477,
    0.764621718684446,
    0.867083638202536,
    1
  )

population <-
  c(
    0.0541619512580634,
    0.118474739059774,
    0.19499841362764,
    0.271513322295285,
    0.342567886789853,
    0.416584281299015,
    0.493129634325766,
    0.570597302980333,
    0.641781270826006,
    0.699804436270574,
    0.757818556828177,
    0.817715444809733,
    0.879858664483404,
    0.939979084328799,
    1
  )

test_that("get_gd_lorenz_params works", {

  params <- get_gd_lorenz_params(
    welfare = welfare,
    population = population)

  # List objects ----------
  expect_equal(names(params), c("lq", "lb", "data"))
  expect_equal(names(params$lq), names(params$lb))
  expect_equal(names(params$lq$reg_results) |>
                 sort(),
               names(params$lb$reg_results) |>
                 sort())

  expect_length(params$lq$reg_results$coef, 3)
  expect_length(params$lb$reg_results$coef, 3)



})

test_that("get_gd_lorenz_validity works", {
  # Using Lorenz parameters from get_gd_lorenz_params
  params <- get_gd_lorenz_params(
    welfare = welfare,
    population = population)

  with_pars <- get_gd_lorenz_validity(params = params)

  # Using welfare and population vecotrs
  with_vecs <- get_gd_lorenz_validity(
    welfare = welfare,
    population = population)

  expect_equal(with_pars, with_vecs)

  # complete results

  with_pars_comp <- get_gd_lorenz_validity(params = params,
                                           complete = TRUE)

  # Using welfare and population vecotrs
  with_vecs_comp <- get_gd_lorenz_validity(
    welfare = welfare,
    population = population,
    complete = TRUE)

  expect_equal(with_pars_comp, with_vecs_comp)

})

test_that("get_gd_select_lorenz works", {

  # regular -----------

  # With parameters
  params <- get_gd_lorenz_validity(
    welfare = welfare,
    population = population,
    complete = TRUE)

  wp <- get_gd_select_lorenz(params = params)
  expect_equal(names(wp), "selected_lorenz")
  expect_equal(names(wp$selected_lorenz),
               c("for_dist", "for_pov", "use_lq_for_dist", "use_lq_for_pov"))

  # With vector
  wv <- get_gd_select_lorenz( welfare = welfare,
                                     population = population)

  expect_equal(wp, wv)

  # Complete -----
  wpc <- get_gd_select_lorenz(params = params,
                                    complete = TRUE)

  wpc_wo_l4 <- wpc
  wpc_wo_l4$selected_lorenz <- NULL

  expect_equal(wpc_wo_l4, params)

  wpc_l4 <- wpc["selected_lorenz"]
  expect_equal(wpc_l4, wp)


})

test_that("get_dg_quantiles works", {

  params <- get_gd_select_lorenz(
    welfare    = welfare,
    population = population,
    complete   = TRUE)

  qt1 <- get_gd_wlf_share_by_qtl(params = params)


  # Using orignal vectors
  qt2 <- get_gd_wlf_share_by_qtl(
  welfare      = welfare,
  population   = population)

  expect_equal(qt1, qt2)

  # force lorenz
  qt3 <- get_gd_wlf_share_by_qtl(
    welfare    = welfare,
    population = population,
    lorenz     = "lb")

  # worng lorenz call
  get_gd_wlf_share_by_qtl(
    welfare    = welfare,
    population = population,
    lorenz     = "kkb") |>
    expect_error()

})
