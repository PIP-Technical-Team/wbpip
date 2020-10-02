# Load list of synthetic test datasets
dl <- readRDS('tests/testdata/synthetic-microdata.RDS')

for (i in seq_along(dl)) {
  m <- dl[[i]]$meta
  filename <- sprintf('md_%s_%s_%s', m$country_code, m$survey_year, m$welfare_type)
  assign(filename, dl[[i]]$data)
}

# Save .rda file to /data
usethis::use_data(md_ABC_2000_income,
                  md_ABC_2010_income,
                  md_DEF_2000_consumption,
                  md_GHI_2000_consumption,
                  md_GHI_2000_income,
                  version = 3, overwrite = TRUE)
