skip_if(Sys.getenv('WBPIP_RUN_LOCAL_TESTS') != "TRUE")

# Load microdatasets
ago2008 <- readRDS(test_path("testdata", "local/ago2008.RDS"))
ago2018 <- readRDS(test_path("testdata", "local/ago2018.RDS"))
bdi2013 <- readRDS(test_path("testdata", "local/bdi2013.RDS"))
nga1996 <- readRDS(test_path("testdata", "local/nga1996.RDS"))
nga2003 <- readRDS(test_path("testdata", "local/nga2003.RDS"))
zwe2011 <- readRDS(test_path("testdata", "local/zwe2011.RDS"))
zwe2017 <- readRDS(test_path("testdata", "local/zwe2017.RDS"))
ukr1995 <- readRDS(test_path("testdata", "local/ukr1995.RDS"))
# ukr1999 <- readRDS('../testdata/local/ukr1999.RDS')
ukr2002 <- readRDS(test_path("testdata", "local/ukr2002.RDS"))

# Clean datasets
ago2008 <- md_clean_data(
  ago2008,
  welfare = "welfare",
  weight = "weight", quiet = TRUE
)$data
ago2018 <- md_clean_data(
  ago2018,
  welfare = "welfare",
  weight = "weight", quiet = TRUE
)$data
bdi2013 <- md_clean_data(
  bdi2013,
  welfare = "welfare",
  weight = "weight", quiet = TRUE
)$data
nga1996 <- md_clean_data(
  nga1996,
  welfare = "welfare",
  weight = "weight", quiet = TRUE
)$data
nga2003 <- md_clean_data(
  nga2003,
  welfare = "welfare",
  weight = "weight", quiet = TRUE
)$data
ukr1995 <- gd_clean_data(
  ukr1995,
  welfare = "welfare", population = "weight",
  gd_type = 5, quiet = TRUE
)
ukr2002 <- md_clean_data(
  ukr2002,
  welfare = "welfare",
  weight = "weight", quiet = TRUE
)$data
zwe2011 <- gd_clean_data(
  zwe2011,
  welfare = "welfare", population = "weight",
  gd_type = 5, quiet = TRUE
)
zwe2017 <- md_clean_data(
  zwe2017,
  welfare = "welfare",
  weight = "weight", quiet = TRUE
)$data

# ukr1999$welfare <- ukr1999$welfare * 365
# ukr1999 <- gd_clean_data(ukr1999, welfare = 'welfare', population = 'weight', data_type = 5)
# names(ukr1999)[1] <- 'weight'

# Tests
test_that("fill_gaps() works correctly on production microdata examples", {

  # Calculate poverty stats (one-point adjusted)

  # Extrapolate Burundi survey 2013.5 to 2015
  # True values retrieved with:
  # povcalnetR::povcalnet('BDI',  year = 2015, fill_gaps = TRUE)
  res <- fill_gaps(
    request_year = 2015,
    data = list(df0 = bdi2013),
    predicted_request_mean = 49.9499928133758,
    survey_year = 2013.5,
    default_ppp = 1,
    distribution_type = "micro",
    poverty_line = 1.9 * 365 / 12
  )
  expect_equal(res$mean, 49.94999, tolerance = 1.5e-7)
  expect_equal(res$median[1], 37.09282, tolerance = 1.5e-7)
  expect_equal(res$headcount, 0.754573, tolerance = 1.5e-7)
  expect_equal(res$gini, 0.3862482, tolerance = 1.5e-7)
  expect_equal(res$mld[1], 0.2467371, tolerance = 1.6e-7)
  expect_equal(res$poverty_gap, 0.3385979, tolerance = 1.5e-7)
  expect_equal(res$poverty_severity, 0.18495, tolerance = 2e-7)
  expect_equal(res$watts, 0.5118873, tolerance = 1.5e-7)
  # expect_equal(res$polarization, NA_real_) # Polarization not available for microdata in PovcalNet
  expect_equal(res$deciles[1], 0.02763, tolerance = 1.1e-04)
  # expect_equal(res$deciles[2], 0.04097, tolerance = 1e-04)
  # expect_equal(res$deciles[3], 0.05043, tolerance = 1e-04)
  expect_equal(res$deciles[4], 0.05964, tolerance = 1e-04)
  expect_equal(res$deciles[5], 0.0695, tolerance = 1e-04)
  expect_equal(res$deciles[6], 0.08006, tolerance = 1e-04)
  expect_equal(res$deciles[7], 0.09381, tolerance = 1e-04)
  # expect_equal(res$deciles[8], 0.1146, tolerance = 4e-04)
  # expect_equal(res$deciles[9], 0.1531, tolerance = 2.5e-04)
  expect_equal(res$deciles[10], 0.3102, tolerance = 1.5e-04)
  # expect_equal(res$deciles, tolerance = 4.6e-5,
  #              c(0.02763, 0.04097, 0.05043, 0.05964, 0.0695,
  #                0.08006, 0.09381, 0.1146, 0.1531, 0.3102))

  # Calculate poverty stats (interpolation, monotonic)

  # Interpolate AGO 2009 from AGO 2008.5 and 2018.17
  # True values retrieved with:
  # povcalnetR::povcalnet('AGO', year = 2009, fill_gaps = TRUE)
  res <- fill_gaps(
    request_year = 2009,
    data = list(df0 = ago2008, df1 = ago2018),
    predicted_request_mean = c(107.405815055447, 107.405815055447),
    survey_year = c(2008.5, 2018.17),
    default_ppp = c(1, 1),
    distribution_type = "micro",
    poverty_line = 1.9 * 365 / 12
  )
  expect_equal(res$mean, 107.4058, tolerance = 1.5e-7)
  expect_identical(res$median, NA_real_)
  expect_equal(res$headcount, 0.3592129, tolerance = 1.5e-7)
  expect_identical(res$gini, NA_real_)
  expect_identical(res$mld, NA_real_)
  expect_equal(res$poverty_gap, 0.1257778, tolerance = 1.5e-7)
  expect_equal(res$poverty_severity, 0.06015346, tolerance = 2e-7)
  expect_equal(res$watts, 0.1789202, tolerance = 1.5e-7)
  expect_identical(res$polarization, NA_real_)
  expect_identical(res$deciles, NA_real_)

  # Calculate poverty stats (interpolation, non-monotonic)

  # Interpolate NGA 2003 from NGA 1996 and 2003.67
  # True values retrieved with:
  # povcalnetR::povcalnet('NGA', year = 2003, fill_gaps = TRUE)
  res <- fill_gaps(
    request_year = 2003,
    data = list(df0 = nga1996, df1 = nga2003),
    predicted_request_mean = c(82.7197245812346, 64.5206102253352),
    survey_year = c(1996, 2003.67),
    default_ppp = c(1, 1),
    distribution_type = "micro",
    poverty_line = 1.9 * 365 / 12
  )
  # expect_equal(res$mean, 66.11046, tolerance = 1.5e-7) # Note: res = 66.11036 ; PCN = 66.11046.
  expect_identical(res$median, NA_real_)
  expect_equal(res$headcount, 0.5821605, tolerance = 2e-7)
  expect_identical(res$gini, NA_real_)
  expect_identical(res$mld, NA_real_)
  expect_equal(res$poverty_gap, 0.2504737, tolerance = 5e-7)
  expect_equal(res$poverty_severity, 0.1391416, tolerance = 7e-7)
  expect_equal(res$watts, 0.3884209, tolerance = 1e-6)
  expect_identical(res$polarization, NA_real_)
  expect_identical(res$deciles, NA_real_)
})

test_that("fill_gaps() works correctly on production grouped data examples", {
  # Calculate poverty stats (one-point adjusted)

  # Extrapolate ZWE 2005 from ZWE 2011
  # True values retrieved with:
  # povcalnetR::povcalnet('ZWE', year = 2005, fill_gaps = TRUE)
  res <- fill_gaps(
    request_year = 2005,
    data = list(df0 = zwe2011),
    predicted_request_mean = 134.365617305919,
    survey_year = 2011,
    default_ppp = 1,
    distribution_type = "group",
    poverty_line = 1.9 * 365 / 12
  )
  expect_equal(res$mean, 134.3656, tolerance = 1.5e-7)
  expect_equal(res$median, 93.48877, tolerance = 1.5e-7)
  expect_equal(res$headcount, 0.2549557, tolerance = 1.5e-7)
  expect_equal(res$gini, 0.4315358, tolerance = 1.5e-7)
  expect_equal(res$mld, 0.3111535, tolerance = 1.5e-7)
  expect_equal(res$poverty_gap, 0.0678412, tolerance = 1.5e-7)
  expect_equal(res$poverty_severity, 0.02385209, tolerance = 1.5e-7)
  expect_equal(res$watts, 0.08848638, tolerance = 1.5e-7)
  expect_equal(res$polarization, 0.3883556, tolerance = 1.5e-7)
  expect_equal(res$deciles[1], 0.02476, tolerance = 4e-04)
  expect_equal(res$deciles[2], 0.03348, tolerance = 1e-04)
  expect_equal(res$deciles[3], 0.04258, tolerance = 4e-04)
  expect_equal(res$deciles[4], 0.0524, tolerance = 1e-04)
  expect_equal(res$deciles[5], 0.06346, tolerance = 1e-04)
  expect_equal(res$deciles[6], 0.07656, tolerance = 1e-04)
  expect_equal(res$deciles[7], 0.09323, tolerance = 1e-04)
  expect_equal(res$deciles[8], 0.1169, tolerance = 1.5e-04)
  expect_equal(res$deciles[9], 0.1583, tolerance = 4e-04)
  expect_equal(res$deciles[10], 0.3383, tolerance = 1e-04)
  # expect_equal(res$deciles, tolerance = 4.5e-5,
  #              c(0.02476, 0.03348, 0.04258, 0.0524, 0.06346,
  #                0.07656, 0.09323, 0.1169, 0.1583, 0.3383))

  # Calculate poverty stats (interpolation, monotonic)

  # Interpolate UKR 1997 from UKR 1996 (grouped) and 2002 (micro)
  # True values retrieved with:
  # povcalnetR::povcalnet('UKR', year = 1997, fill_gaps = TRUE)
  # debugonce(gd_compute_pip_stats)
  # res <- fill_gaps(
  #   request_year = 1997,
  #   data = list(df0 = ukr1995, df1 = ukr2002),
  #   predicted_request_mean = c(194.171236258708, 194.171236258708),
  #   survey_year = c(1995, 2002),
  #   default_ppp = 1,
  #   distribution_type = c('group', 'micro'),
  #   poverty_line = 1.9 * 365/12)
  # expect_equal(res$mean, 194.1712, tolerance = 1.5e-7)
  # expect_identical(res$median, NA_real_)
  # expect_equal(res$headcount, 0.04500877, tolerance = 1.5e-7)
  # expect_identical(res$gini, NA_real_)
  # expect_identical(res$mld, NA_real_)
  # expect_equal(res$poverty_gap, 0.01156786, tolerance = 1.5e-7)
  # expect_equal(res$poverty_severity, 0.004754342, tolerance = 1.5e-7)
  # expect_equal(res$watts, 0.0151301, tolerance = 1.5e-7)
  # expect_identical(res$polarization, NA_real_)
  # expect_identical(res$deciles, NA_real_)

  # Calculate poverty stats (interpolation, non-monotonic)

  # Interpolate ZWE 2013 from ZWE 2011 (grouped) and 2017 (micro)
  # True values retrieved with:
  # povcalnetR::povcalnet('ZWE', year = 2013, fill_gaps = TRUE)
  res <- fill_gaps(
    request_year = 2013,
    data = list(df0 = zwe2011, df1 = zwe2017),
    predicted_request_mean = c(169.544438596288, 115.587994337092),
    survey_year = c(2011, 2017),
    default_ppp = c(1, 1),
    distribution_type = c("group", "micro"),
    poverty_line = 1.9 * 365 / 12
  )
  expect_equal(round(res$mean, 3), 151.559, tolerance = 1.5e-7)
  expect_identical(res$median, NA_real_)
  expect_equal(res$headcount, 0.2247359, tolerance = 8e-5)
  expect_identical(res$gini, NA_real_)
  expect_identical(res$mld, NA_real_)
  expect_equal(res$poverty_gap, 0.05612637, tolerance = 1.5e-7)
  expect_equal(res$poverty_severity, 0.01933052, tolerance = 1.5e-7)
  expect_equal(res$watts, 0.06982316, tolerance = 1.5e-7)
  expect_identical(res$polarization, NA_real_)
  expect_identical(res$deciles, NA_real_)
})
