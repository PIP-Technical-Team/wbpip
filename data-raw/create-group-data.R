

# National grouped data (type 5)
gd_GHI_2009_income <- data.frame(
  country_code = 'JKL',
  survey_year = 2009,
  weight = 10,
  welfare = c(18.10, 27.50, 35.20, 43.30, 51.61, 56.15, 75.17, 93.39, 128.30, 260.90),
  area = ''
)
usethis::use_data(gd_GHI_2009_income, overwrite = TRUE)
