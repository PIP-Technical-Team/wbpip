
#' Title
#'
#' @param .data Household survey dataframe with at least a welfare variable
#' @inheritParams md_clean_data
#' @inheritParams md_compute_gini
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
#' get_gini(dt, welfare, weight)
#'
#' setnames(md_ABC_2000_income, c("welfare", "weight"), c("wf", "wt") )
#'
#' md_ABC_2000_income %>%
#'   group_by(area) %>%
#'   get_gini(wf, wt)
#'
#' md_ABC_2000_income %>%
#'   get_gini(wf, wt)
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
               type = type,
               welfare = welfare,
               weight  = weight)

  # Clean data
  df <- do.call(md_clean_data, args)$data

  # Compute Gini
  gini <- md_compute_gini(df[[welfare]] , df[[weight]])

  # return(gini)
  return(tibble::tibble(gini = gini))
}

