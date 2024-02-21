#' Compute Poverty Statistics
#'
#' Compute poverty statictics for microdata - replace old [md_compute_poverty_stats]
#'
#' Given a vector of consumption or income values and their respective weights
#' `md_compute_poverty_stats()` computes poverty headcount, poverty gap,
#' poverty severity and the watts index.
#'
#' @inheritParams compute_pip_stats
#' @inheritParams md_compute_headcount
#' @param povline_lcu numeric: Poverty line in Local Currency Unit (LCU).
#'
#' @examples
#' wbpip:::md_compute_poverty_stats(
#'   welfare = 1:2000,
#'   weight = rep(1, 2000),
#'   povline_lcu = 10
#' )
#' @return list
#' @export
md_compute_poverty_stats <- function(
    welfare,
    weight,
    povline_lcu
) {


  # ______________________________________________________________________
  # FGT measures
  # ______________________________________________________________________

  fgt <- md_compute_fgt(welfare     = welfare,
                        weight      = weight,
                        povline     = povline_lcu,
                        return_data =  TRUE) |>
    md_compute_fgt(alpha = 1,
                   return_data =  TRUE) |>
    md_compute_fgt(alpha = 2,
                   return_data =  TRUE)

  hc <- fgt$FGT0
  pg <- fgt$FGT1
  ps <- fgt$FGT2

  watts <- md_compute_watts(
    welfare           = welfare,
    weight            = weight,
    povline           = povline_lcu
  )

  # ______________________________________________________________________
  # Return list
  # ______________________________________________________________________
  return(
    list(
      headcount        = hc,
      poverty_gap      = pg,
      poverty_severity = ps,
      watts            = watts
    )
  )
}





#' Compute FGT poverty family measures and Watts index for Microdata
#'
#' @param fgt_data list of previously computed fgt calculations
#' @param welfare numeric vector with either income or consumption
#' @param weight numeric vector with sample weights. Default is 1.
#' @param povline poverty line. Default is the half the weighted median of
#'   `welfare`
#' @param alpha numeric. Alpha parameter of FGT measures. if `0`, the default,
#'   it estimates the poverty headcount. If `1`, the poverty gap, and if `2`,
#'   the poverty severity. In practice, you can use higher levels of `alpha`,
#'   but their theoretical interpretation usually goes up to a value of `2`.
#' @param return_data logical: whether to return a list to be used in subsequent
#'   calls of [md_compute_fgt] in the parameter `fgt_data`.
#' @param include_povline logical: Whether to include the poverty line as
#'   threshold for poverty measure. The default is `FALSE`, as absolute poverty
#'   is defined as those household *below* the poverty line. Yet, it might be
#'   useful to include the value of the poverty line for a very limited set of
#'   analysis (*seldom used*).
#'
#' @details [md_compute_fgt] works in two ways. It could either receive a list
#'   of previously computed calculations in argument `fgt_data` or receive the
#'   standard poverty calculation inputs such as `welfare`, `weights` and
#'   `povline`. The first modality ensures efficiency in computations as the
#'   poverty status of each observation and their relative distance to the
#'   poverty line is calculated only once.
#'
#' @section wrappers:
#'
#'   There are a few functions that are basically wrappers of [md_compute_fgt].
#'   They do not serve any purpose beyond ease to the user to identify the right
#'   measure.
#'
#'   [md_compute_headcount] Computes poverty headcount, which is equivalent to
#'   `md_compute_fgt(alpha = 0)`
#'
#'   [md_compute_pov_gap]   Computes poverty gap, which is equivalent to
#'   `md_compute_fgt(alpha = 1)`
#'
#'   [md_compute_pov_severity] Computes poverty severity, which is equivalent to
#'   `md_compute_fgt(alpha = 2)`
#'
#'   [md_compute_watts] is not a wrapper of [md_compute_fgt] but it is part of
#'   the poverty measures, so it is included in this documentation. Notice that
#'   the arguments are the same as of the functions above.
#'
#' @section inclusion of poverty line: when `include_povline` is `TRUE`, the
#'   value of the `povline` is artificially modify to `povline + e` where `e` is
#'   a very small number (`1e-10`), ensure the inclusion of the line.
#'
#'
#' @return either a vector with the fgt measure selected in argument `alpha` or
#'   a list of dgt estimations if `return_data` is `TRUE`
#' @export
#'
#' @examples
#' welfare <- md_ABC_2010_income$welfare/1e6
#' weight  <- md_ABC_2010_income$weight
#'
#' wna     <- !is.na(welfare)
#' welfare <- welfare[wna]
#' weight  <- weight[wna]
#'
#' md_compute_fgt(welfare = welfare,
#'                weight  = weight,
#'                povline = 5)
#'
#' fgt <- md_compute_fgt(welfare     = welfare,
#'                       weight      = weight,
#'                       povline     = 5,
#'                       return_data =  TRUE) |>
#'   md_compute_fgt(alpha = 1,
#'                  return_data =  TRUE) |>
#'   md_compute_fgt(alpha = 2,
#'                  return_data =  TRUE)
#'
#' c(fgt$FGT0, fgt$FGT1, fgt$FGT2)
md_compute_fgt <- function(fgt_data        = NULL,
                           welfare         = NULL,
                           weight          = rep(1, length(welfare)),
                           povline         = fmedian(welfare, w = weight)/2,
                           alpha           = 0,
                           return_data     = FALSE,
                           include_povline = FALSE
                           ) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Defenses --------
    if (is.null(fgt_data) && is.null(welfare) ||
        !is.null(fgt_data) && !is.null(welfare)) {
      cli::cli_abort("You must provide either {.arg fgt_data} of {.arg welfare}")
    }
  stopifnot(length(povline) == 1) # should we vectorize this?

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  if (include_povline) {
    povline <- povline + 1e-10
  }
  if (is.null(fgt_data)) {
    fgt_data <- vector("list", length = 3)
    names(fgt_data) <- c("pov_status", "relative_distance", "weight")

    fgt_data$pov_status         <-  welfare < povline
    fgt_data$relative_distance  <- 1 - (welfare / povline)
    fgt_data$weight             <- weight

  }

  x <-
    ((fgt_data$pov_status) * (fgt_data$relative_distance)^alpha) |>
    fmean(w = fgt_data$weight)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (return_data) {
    fgt_name <- paste0("FGT",alpha)
    fgt_data[[fgt_name]] <- x
    return(fgt_data)
  }

  x
}

#' @rdname md_compute_fgt
#' @export
md_compute_headcount <- function(
    welfare,
    weight          = rep(1, length(welfare)),
    povline         = fmedian(welfare, w = weight)/2,
    return_data     = FALSE,
    include_povline = FALSE
){


  md_compute_fgt(welfare         = welfare,
                 weight          = weight,
                 povline         = povline,
                 alpha           = 0,
                 include_povline = include_povline)

}


#' @rdname md_compute_fgt
#' @export
md_compute_pov_gap <- function(
    welfare,
    weight          = rep(1, length(welfare)),
    povline         = fmedian(welfare, w = weight)/2,
    return_data     = FALSE,
    include_povline = FALSE
) {
  md_compute_fgt(welfare         = welfare,
                 weight          = weight,
                 povline         = povline,
                 alpha           = 1,
                 include_povline = include_povline)
}

#' @rdname md_compute_fgt
#' @export
md_compute_pov_severity <- function(
    welfare,
    weight          = rep(1, length(welfare)),
    povline         = fmedian(welfare, w = weight)/2,
    return_data     = FALSE,
    include_povline = FALSE
) {
  md_compute_fgt(welfare         = welfare,
                 weight          = weight,
                 povline         = povline,
                 alpha           = 2,
                 include_povline = include_povline)
}

#' @rdname md_compute_fgt
#' @export
md_compute_watts <- function(
    welfare,
    weight          = rep(1, length(welfare)),
    povline
) {

  ss_args <- environment() |>
    as.list()

  null_args <- sapply(ss_args, is.null)

  if (any(null_args)) {
    cli::cli_abort("{.or {.arg  {names(ss_args)}}} can't be NULL")
  }


  # ______________________________________________________________________
  # Computations
  # ______________________________________________________________________
  pov_status         <- (welfare < povline)
  weight_total       <- fsum(weight)
  keep               <- welfare > 0 & pov_status
  w_gt_zero          <- welfare[keep]
  sensitive_distance <- log(povline / w_gt_zero)
  watts              <- fsum(sensitive_distance * weight[keep])/weight_total

  # Handle cases where Watts is numeric(0)
  if (identical(watts, numeric(0))) {
    watts <- 0
  }

  # ______________________________________________________________________
  # Return
  # ______________________________________________________________________
  return(watts)
}

