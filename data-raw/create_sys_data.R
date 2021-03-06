empty_gd_compute_pip_stats_response <- list(
  gini = NA,
  median = NA,
  rmhalf = NA,
  polarization = NA,
  ris = NA,
  mld = NA,
  dcm = NA,
  deciles = rep(NA, 10), # Potential issue here as I am hard coding deciles, when the function can theoretically return any quantiles
  headcount = NA,
  poverty_gap = NA,
  poverty_severity = NA,
  eh = NA,
  epg = NA,
  ep = NA,
  gh = NA,
  gpg = NA,
  gp = NA,
  watts = NA,
  dl = NA,
  ddl = NA,
  is_normal = FALSE,
  is_valid = FALSE
)

usethis::use_data(empty_gd_compute_pip_stats_response,
                  overwrite = TRUE,
                  internal  = TRUE)
