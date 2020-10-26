#' get_lorenz
#'
#' Use function `md_compute_lorenz()` to calculate quantiles. In addition, this
#' function makes sure data is properly cleaned using `md_clean_data`
#'
#' @param .data Household survey dataframe with at least a welfare variable
#' @inheritParams md_compute_lorenz
#' @param type character: either 'microdata' or 'groupdata'. Defatult 'microdata'
#'
#' @return data.frame
#' @export
#' @import data.table
#'
#' @examples
#' data("md_ABC_2000_income")
#' df     <- md_ABC_2000_income
#' lorenz <- get_lorenz(df, welfare, weight)
#' str(lorenz)
get_lorenz <-  function(.data,
                        welfare,
                        weight,
                        type   = "microdata",
                        nbins  = NULL) {

  if (inherits(.data, "grouped_df")) {

    # Organize argument to parse to md_clean_data
    args_in <- list(welfare = substitute(welfare),
                    weight  = substitute(weight),
                    type    = type,
                    nbins   = nbins)

    return(dplyr::do(.data,
                     do.call(get_lorenz,c(list(.data =.), args_in)
                     )
    )
    )
  }

  # Check arguments

  welfare <- deparse(substitute(welfare))
  weight  <- deparse(substitute(weight))


  # Organize argument to parse to md_clean_data
  args <- list(dt      = .data,
               welfare = welfare,
               weight  = weight)

  if (type == "microdata") {
    # Clean data
    df <- do.call(md_clean_data, args)$data

    # Compute Lorenz
    df_lorenz <- md_compute_lorenz(df[[welfare]],
                                 df[[weight]],
                                 nbins  = nbins)



  } else if (type == "groupdata") {

    rlang::inform("process for group data not ready yet")
    gini <- NA

  } else {
    msg     <- "Wrong `type`"
    hint    <- "Make sure `type` is either 'microdata' or 'group data'"
    problem <- paste("your `type` is", type)
    rlang::abort(c(
      msg,
      i = hint,
      x = problem
    ),
    class = "wbpip_error"
    )

  }

  return(df_lorenz)
}
