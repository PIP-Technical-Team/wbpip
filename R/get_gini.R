#' @import data.table
NULL

# Add global variables to avoid NSE notes in R CMD check
if (getRversion() >= '2.15.1')
  utils::globalVariables(
    c('.')
  )

#' Gini coefficient
#'
#' Compute the Gini coefficient.
#'
#' @param .data Household survey data frame with at least a welfare variable.
#' @inheritParams md_compute_gini
#' @param distribution_type character: Type of distribution, either micro,
#'   group, aggregate or imputed.
#'
#' @return data.table
#' @export
get_gini <- function(.data, welfare, weight,
                     distribution_type =
                       c("micro", "group", "aggregate", "imputed")) {

  # Match arg
  distribution_type <- match.arg(distribution_type)

  if (inherits(.data, "grouped_df")) {

    # Organize argument to parse to md_clean_data
    args_in <- list(welfare = substitute(welfare),
                    weight  = substitute(weight),
                    distribution_type = distribution_type)

    return(dplyr::do(.data,
                     do.call(get_gini,c(list(.data = .), args_in)
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

  if (distribution_type == "micro") {
    # Clean data
    df <- do.call(md_clean_data, args)$data

    # Compute Gini
    gini <- md_compute_gini(df[[welfare]] , df[[weight]])

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
  return(data.table::data.table(gini = gini))
}

