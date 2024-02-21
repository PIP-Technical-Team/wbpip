#' wbpip: Methodological worlkflow of PIP
#'
#' Basic workflows are based on single data files (micro- or grouped-data). The
#' assumption for these workflows is that they can be satisfied by feeding a
#' single micro- / grouped-data file to different functions
#'
#' @section wbpip functions: The wbpip are divided in...
#'
#' @docType package
#' @name wbpip
#' @rawNamespace import(collapse, except = fdroplevels)
#' @rawNamespace import(data.table, except = fdroplevels)
#' @importFrom cli cli_abort cli_warn cli_inform cli_alert cli_alert_danger
#'   cli_alert_info cli_alert_success

# Make sure data.table knows we know we're using it
#' @noRd
.datatable.aware = TRUE

# Prevent R CMD check from complaining about the use of pipe expressions
# standard data.table variables
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    names = c(
      ".",
      ".I",
      ".N",
      ".SD",
      ".",
      "!!",
      ":="
    ),
    package = utils::packageName()
  )
}

#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
