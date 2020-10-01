#' Check microdata to be used in PIP methods
#'
#' @param dt Data frame.
#' @param ... list of arguments that correspond to specific variable names.
#' Arguments available are in `getOption("wbpip.agrs_to_check")`.
#' For instance, welfare = "income", weight = "peso'.
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
md_check_data <- function(dt, ...) {


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #---------   SET UP   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  agrs_to_check <- getOption("wbpip.agrs_to_check")

  # parse ellipsis as a list, where names are the the names of
  # the arguments, and contents the names of the variables to
  # be evaluated.
  cols     <- eval(substitute(alist(...)))
  # cols  <- as.list(match.call())[-(1:2)]
  # cols  <- as.list(substitute(list(...)))[-1L]


  # Name of columns
  colnames <- unlist(cols, use.names = FALSE)

  # Names of arguments
  argnames <- names(cols)

  #--------- check that all the variables selected exist in dt ---------

  if (!(all(colnames %in% names(dt)))) {

    nocolind <- which(!(colnames %in% names(dt)))
    nocols   <- paste0("`", colnames[nocolind], "`")
    nocols   <- last_item(nocols)


    msg     <- "Variables provided do not exist in data"
    hint    <- "Make sure the variables you want to evaluate exist in dataframe"
    problem <- paste("variable(s)", nocols, "is(are) not part of the dataframe")
    rlang::abort(c(
                  msg,
                  i = hint,
                  x = problem
                  ),
                  class = "wbpip_error"
                  )

  }

  #--------- check that the names of the arguments are part of the variables to check ---------
  if (!(all(argnames %in% agrs_to_check))) {

    nocolind <- which(!(argnames %in% agrs_to_check))
    nocols   <- paste0("`", argnames[nocolind], "`")
    nocols   <- last_item(nocols)

    args2   <- paste0("`", agrs_to_check, "`")
    args2   <- last_item(args2)


    msg     <- "Arguments provided are not valid"
    hint    <- paste("Make sure the arguments are at least one among", args2)
    problem <- paste("Argument(s)", nocols, "is(are) not valid")
    rlang::abort(c(
      msg,
      i = hint,
      x = problem
    ),
    class = "wbpip_error"
    )

  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #---------   CHECKs by variable   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #--------- WELFARE ---------

  if ("welfare" %in% argnames) {
    welf <- cols$welfare

    # Check for missing values
    nna <- dt[is.na(get(welf)) , .N]
    if (nna > 0) {

      msg     <- paste("NA values in", welf, "will be dropped")
      hint    <- paste("you have", nna, "values in", welf)
      rlang::inform(c(
                    msg,
                    i = hint
                    )
                    )

      dt <- dt[!is.na(get(welf))]

    }

    # Check for negative values
    nng <- dt[get(welf) < 0 , .N]
    if (nng > 0) {

      msg     <- paste("Negative values in", welf, "will be dropped")
      hint    <- paste("you have", nng, "values in", welf)
      rlang::inform(c(
        msg,
        i = hint
      )
      )

      dt <- dt[get(welf) >= 0]
    }

    # Check for super outliers (unweigthed)
    sdmn <- dt[, .(sd   = sd(get(welf), na.rm = TRUE),
                   mean = mean(get(welf), na.rm = TRUE))
               ]

    # outliers
    ol <- dt[get(welf) > 4*sdmn[, sd] + sdmn[, mean], .N]

    # Super outliers
    sol <- dt[get(welf) > 8*sdmn[, sd] + sdmn[, mean], .N]

    if (ol > 0) {
      rlang::inform(paste0("you have ", ol, " outliers in `", welf,
                          "` that are greater than 4 times the SD of the mean"))
    }

    if (sol > 0) {
      rlang::inform(paste0("you have ", sol, " outliers in `", welf,
                          "` that are greater than 8 times the SD of the mean"))
    }

  }




  return(cols)
}

