#' Gini coefficient
#'
#' Compute the Gini coefficient from microdata
#'
#' @param .data Household survey dataframe with at least a welfare variable
#' @inheritParams md_compute_gini
#' @param type character: either 'microdata' or 'groupdata'. Defatult 'microdata'
#'
#' @return data.frame
#' @export
#' @import data.table
#'
get_gini <-  function(.data, welfare, weight, type = "microdata") {

  if (inherits(.data, "grouped_df")) {

    # Organize argument to parse to md_clean_data
    args_in <- list(welfare = substitute(welfare),
                    weight  = substitute(weight),
                    type = type)

    return(dplyr::do(.data,
                     do.call(get_gini,c(list(.data =.), args_in)
                             )
                     )
           )
  }

  # Check arguments

  welfare <- deparse(substitute(welfare))
  weight  <- deparse(substitute(weight))


  # Organize argument to parse to md_clean_data
  args <- list(dt = .data,
               welfare = welfare,
               weight  = weight)

  if (type == "microdata") {
    # Clean data
    df <- do.call(md_clean_data, args)$data

    # Compute Gini
    gini <- md_compute_gini(df[[welfare]] , df[[weight]])

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

  # return(gini)
  return(tibble::tibble(gini = gini))
}

