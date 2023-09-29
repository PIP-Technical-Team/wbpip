#' @keywords internal
"_PACKAGE"

#' wbpip: Official methodological Stats of PIP
#'
#' wbpip is the main methodological engine of the PIP API
#'
#' @section wbpip functions:
#' The wbpip functions ...
#'
#' @docType package
#' @name wbpip
#'
# Make sure data.table knows we know we're using it
#' .datatable.aware = TRUE

# Prevent R CMD check from complaining about the use of pipe expressions
# standard data.table variables
# if (getRversion() >= "2.15.1") {
#   utils::globalVariables(
#     names = c(
#       ".",
#       ".I",
#       ".N",
#       ".SD",
#       ".",
#       "!!",
#       ":=",
#       "..output"
#     ),
#     package = utils::packageName()
#   )
# }

## usethis namespace: start
#' @importFrom lifecycle deprecated
#' @import collapse
## usethis namespace: end
#' @noRd
NULL
