## testing that the get lorenz function creates expected outputs

test_that("testing that the get_lorenz function is producing expected results under microdata assumption", {

  test_dt <- readRDS("../testdata/synthetic-microdata.RDS")

  welfare_dt <- data.table::as.data.table(test_dt[[1]]$data)

  mdlorenz <- get_lorenz(.data = welfare_dt,
                         welfare = welfare,
                         weight = weight,
                         nbins = 100)

  expected_dt <- test_dt[[1]]$stats$lorenz
  names(expected_dt) <- c("welfare", "lorenz_weight", "lorenz_welfare")

  expect_equal(mdlorenz, expected_dt[,c("welfare", "lorenz_welfare", "lorenz_weight")])


})

# test_that("testing that the get_lorenz function works as expected under the group data assumption", {
#
#   test_dt <- readRDS("../testdata/gd_ex1.RDS")
#
#   mdlorenz <- get_lorenz(.data = test_dt,
#                          welfare = welfare,
#                          weight = weight,
#                          distribution = "group",
#                          nbins = 3)
#
# })
