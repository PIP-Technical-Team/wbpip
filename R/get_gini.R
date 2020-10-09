
#' Title
#'
#' @param .data
#' @param welfare
#' @param weight
#' @param type
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
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

