#' Check microdata to be used in PIP methods
#'
#' @param dt Data frame.
#' @param ... list of arguments that correspond to specific variable names.
#' Arguments available are in `getOption("wbpip.agrs_to_check")`.
#' For instance, welfare = "income", weight = "peso'.
#'
#' @return list of elements whose main object is a dataframe (in data.table format)
#' with the necessary transformations to be included in PIP methods. Data is available in
#' element $data. The other elements provide the number of observations that were modified
#' depending on test performed. The name of elements are in the form p_s, where p (or prefix)
#' refers to the test, whereas s (for suffix) refers to the name of the variable evaluated.
#' Prefix are:
#'
#' *nna:*  Number of NA in variable
#' *nng:*  Number of negative values
#' *ina:*  Index of obs with NA in variable
#' *ing:*  Index of obs with negative values
#' @export
#' @import data.table
#'
md_clean_data <- function(dt, ...) {

  if(!(inherits(dt, "data.table"))) {
    setDT(dt)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #--------- SET UP   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  agrs_to_check <- getOption("wbpip.agrs_to_check")

  # List to return
  ll <- vector(mode = "list")

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

  #--------- Check that all the variables selected exist in dt ---------

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

  #--------- Check that the names of the arguments are part of the variables to check ---------
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
  #--------- CHECKs by variable   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #--------- WELFARE ---------

  if ("welfare" %in% argnames) {
    welf <- as.character(cols$welfare)

    # Check for missing values
    nna <- dt[is.na(get(welf)) , .N]

    if (nna > 0) {
      ina <- dt[, which(is.na(get(welf)))]

      dt  <- dt[!is.na(get(welf))] # remove values

      ll[[paste0("nna_", welf)]] <- nna
      ll[[paste0("ina_", welf)]] <- ina

      nna_msg(nna, welf)
    }

    # Check for negative values
    nng <- dt[get(welf) < 0 , .N]
    if (nng > 0) {

      ing <- dt[, which(get(welf) < 0)]

      dt <- dt[get(welf) >= 0]

      ll[[paste0("nng_", welf)]] <- nng
      ll[[paste0("ing_", welf)]] <- ing

      nng_msg(nng, welf)
    }

  } # End of welfare check

  #--------- WEIGHT ---------
  if ("weight" %in% argnames) {

    wht <- as.character(cols$weight)

    # Check for missing values
    nna <- dt[is.na(get(wht)) , .N]

    if (nna > 0) {

      ina <- dt[, which(is.na(get(wht)))]
      dt <- dt[!is.na(get(wht))]

      ll[[paste0("nna_", wht)]] <- nna
      ll[[paste0("ina_", wht)]] <- ina

      nna_msg(nna, wht)
    }

    # Check for negative values
    nng <- dt[get(wht) <= 0 , .N]

    if (nng > 0) {
      ing <- dt[, which(get(wht) <= 0)]
      dt <- dt[get(wht) > 0]

      ll[[paste0("nng_", wht)]] <- nng
      ll[[paste0("ing_", wht)]] <- ing

      nng_msg(nng, wht)

    }
  } # end of weight check

  ll[["data"]] <- dt
  return(ll)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#---------   Messages   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

nng_msg <- function(nng, x) {
  msg     <- paste0(nng, " zero or negative values in variable `", x, "` were dropped")
  rlang::inform(c(i = msg))
  invisible()
}

nna_msg <- function(nna, x) {
  msg     <- paste0(nna, " NA values in variable `", x, "` were dropped")
  rlang::inform(c(i = msg))
  invisible()
}
