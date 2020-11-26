# Add global variables to avoid NSE notes in R CMD check
if (getRversion() >= '2.15.1')
  utils::globalVariables(
    c('type', '.')
  )

#' Bins
#'
#' Calculate bins. **TO BE DOCUMENTED**.
#'
#' @param .data Household survey data frame with at least a welfare variable.
#' @inheritParams md_compute_bins
#' @param distribution_type character: Type of distribution, either micro,
#'   group, aggregate or imputed.
#'
#' @examples
#' data("md_ABC_2000_income")
#' df <- md_ABC_2000_income
#'
#' bins <- get_bins(df, welfare, weight)
#' str(bins)
#'
#' bins <- get_bins(df, welfare, weight, output = "full")
#' str(bins)
#'
#' @return data.frame
#' @export
get_bins <-  function(.data,
                      welfare,
                      weight,
                      distribution_type = c("micro", "group", "aggregate", "imputed"),
                      nbins  = 100,
                      output = "simple") {

  # Match arg
  distribution_type <- match.arg(distribution_type)

  if (inherits(.data, "grouped_df")) {

    # Organize argument to parse to md_clean_data
    args_in <- list(welfare = substitute(welfare),
                    weight  = substitute(weight),
                    type    = type,
                    nbins   = nbins,
                    output  = output)

    return(dplyr::do(.data,
                     do.call(get_bins,c(list(.data = .), args_in)
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

    # Compute Bins
    df_bins <- md_compute_bins(df[[welfare]],
                               df[[weight]],
                               nbins  = nbins,
                               output = output)

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

  # return(gini)
  return(df_bins)
}
