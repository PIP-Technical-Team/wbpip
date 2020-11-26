#' Lorenz curve
#'
#' Calculate points on the Lorenz curve. **TO BE DOCUMENTED**.
#'
#' @param .data Household survey data frame with at least a welfare variable.
#' @inheritParams md_compute_lorenz
#' @param distribution_type character: Type of distribution, either micro,
#'   group, aggregate or imputed.
#'
#' @examples
#' data("md_ABC_2000_income")
#' df     <- md_ABC_2000_income
#' lorenz <- get_lorenz(df, welfare, weight)
#' str(lorenz)
#'
#' @return data.frame
#' @export
get_lorenz <-  function(.data,
                        welfare,
                        weight,
                        distribution_type = c("micro", "group", "aggregate", "imputed"),
                        nbins  = NULL) {

  # Match arg
  distribution_type <- match.arg(distribution_type)

  if (inherits(.data, "grouped_df")) {

    # Organize argument to parse to md_clean_data
    args_in <- list(welfare = substitute(welfare),
                    weight  = substitute(weight),
                    type    = type,
                    nbins   = nbins)

    return(dplyr::do(.data,
                     do.call(get_lorenz,c(list(.data = .), args_in)
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

  if (distribution_type == "micro") {
    # Clean data
    df <- do.call(md_clean_data, args)$data

    # Compute Lorenz
    df_lorenz <- md_compute_lorenz(df[[welfare]],
                                   df[[weight]],
                                   nbins  = nbins)

  } else if (distribution_type == "group") {

    rlang::inform("process for group data not ready yet")
    gini <- NA

  } else {
    msg     <- "Wrong `distribution_type`"
    hint    <- "Make sure `distribution_type` is either 'micro' or 'group'"
    problem <- paste("your `distribution_type` is", distribution_type)
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
