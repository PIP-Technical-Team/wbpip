# testing the get bins function


test_that("get bins produces expected outputs", {

  # Read in synthetic microdata
  dl <- readRDS('../testdata/synthetic-microdata.RDS')

  dl <- dl[[1]]$data

  bins <- get_bins(dl, welfare = welfare, weight = weight)

  #test that the correct number of bins
  testthat::expect_equal(length(unique(bins$bins)), 100)

  #I cant really test what happens when distribution type is "group" because it also kicks out an error message

  ## "Error in get_bins(dl, welfare = welfare, weight = weight, distribution_type = "group") :
  ## object 'df_bins' not found"

})
