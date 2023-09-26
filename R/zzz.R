wbpip_default_options <- list(
  wbpip.verbose = TRUE,
  wbpip.available_ppp_years = c(2017, 2011)
)

.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(wbpip_default_options) %in% names(op))
  if (any(toset)) options(wbpip_default_options[toset])

  invisible()
}


