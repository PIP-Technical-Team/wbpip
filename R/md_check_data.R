library(devtools)
library(pipload)
library(data.table)

dt <- pip_load_data(country = "PRY",
                    year    = 2017,
                    tool    = "TB")


md_check_data <- function(dt, ...
                          # welfare   = NULL,
                          # weight    = NULL,
                          # age       = NULL,
                          # educy     = NULL,
                          # educat4   = NULL,
                          # educat5   = NULL,
                          # educat7   = NULL
                          ) {

  # cols  <- as.list(match.call())[-(1:2)]
  # cols  <- as.list(substitute(list(...)))[-1L]

  cols     <- eval(substitute(alist(...))) # columns as symbols
  colnames <- unlist(cols, use.names = FALSE)

  # check that all the variables selected exist in dt



  # check that the names of the arguments are part of the variables to check



  if ("welfare" %in% names(cols)) {
    welf <- cols$welfare


    dt[is.na(get(welf)) , .N]
  }

  return(cols)
}


cols <- md_check_data(dt,
                      welfare = "welfare",
                      weight  = "weight")

cols <- md_check_data(dt,
                      welfare = "bla",
                      weight  = "ble")

