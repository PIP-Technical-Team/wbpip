pipuax_default_options <- list(
  wbpip.agrs_to_check  = c("welfare",
                           "weight",
                           "age",
                           "educy",
                           "educat4",
                           "educat5",
                           "educat7")

)

.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(pipuax_default_options) %in% names(op))
  if (any(toset)) options(pipuax_default_options[toset])

  invisible()
}
