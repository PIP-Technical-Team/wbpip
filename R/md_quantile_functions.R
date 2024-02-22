# md quantile functions that will be used in {pipster}




#' Get quantile at specified shared of population - micro data (old version)
#'
#' `old_md_quantile_values` returns the quantile (i.e., monetary value) that corresponds
#' to share of the population that lives below that threshold.
#'
#' This is basically the inverse of estimating the poverty rate (headcount or
#' population share) below the poverty line. In this case, you provide the
#' headcount and `md_quantile_values` returns the "poverty line".
#'
#' The quantiles are calculated as function of the mean of the distribution
#' times an `x` factor. Basically, the quantile is `x` times the mean. By
#' default, the mean is equal to 1, which implies that, if no mean value if
#' provided, the return value is equal to `x`.
#'
#' @param welfare welfare vector
#' @param weight population weight vector. Default is 1
#' @param n numeric: number of equi-spaced quantiles
#' @param popshare numeric atomic vector: the quantiles to return. Will only be
#' used if `n = NULL`, else will be vector determined by the n equi-spaced quantiles.
#' @param format character: "dt", "list", "atomic", giving the format of the
#' output
#'
#' @return quantiles: see `format`
#' @export
#'
#' @examples
#' old_md_quantile_values(
#'   welfare = md_GHI_2000_consumption$welfare,
#'   weight  = md_GHI_2000_consumption$weight,
#'   n       = 5
#' )
old_md_quantile_values <- function(
    welfare    = NULL,
    weight     = rep(1, length = length(welfare)),
    n          = NULL,
    popshare   = ifelse(is.null(n),
                        seq(from = 1/10, to = 1, by = 1/10),
                        seq(from = 1/n, to = 1, by = 1/n)),
    format     = c("dt", "list", "atomic")
){

  # ____________________________________________________________________________
  # Arguments ------------------------------------------------------------------
  if (is.na(welfare) |> any()) {
    cli::cli_abort("No elements in welfare vector can be NA")
  }
  if (is.null(welfare)) {
    cli::cli_abort("Welfare vector cannot be NULL")
  }
  if (length(weight) > 1 & any(is.na(weight))) {
    cli::cli_abort("No elements in weight vector can be NA - make NULL to use equal weighting")
  }
  if (is.null(n) & is.null(popshare)) {
    cli::cli_abort("Either `n` or `popshare` must be non-NULL")
  }
  format <- match.arg(format)

  # ____________________________________________________________________________
  # Validate n ----------------------------------------------------------
  if (!is.null(n)) {
    popshare <- seq(from = 1/n, to = 1, by = 1/n)
  }

  # ----------------------------------------------------------------------------
  # Validate popshare ----------------------------------------------------------
  if (!is.null(popshare)) {
    if (any(popshare < 0 | popshare > 1)) {
      cli::cli_abort("popshare must be within the range [0, 1]")
    }
  }

  # ____________________________________________________________________________
  # Calculations ---------------------------------------------------------------
  q <- fquantile(
    x     = welfare,
    w     = weight,
    probs = popshare
  )

  # ____________________________________________________________________________
  # Format and Return ----------------------------------------------------------
  if (format == "atomic") {
    return(q)
  } else if (format == "dt") {
    q <- data.table::data.table(
      quantile = paste0("q_", names(q)),
      values   = q |> as.numeric()
    )
    return(q)
  } else if (format == "list") {
    return(
      as.list(q)
    )
  }

}

#' Get quantile at specified shared of population - micro data
#'
#' `md_quantile_values` returns the quantile (i.e., monetary value) that corresponds
#' to the share of the population that lives below that threshold.
#'
#' The quantiles are calculated using the `welfare` outcome of the function
#' `md_compute_lorenz`.
#'
#' @param welfare welfare vector
#' @param weight population weight vector. Default is 1
#' @param n numeric: number of equi-spaced quantiles
#' @param format character: "dt", "list", "atomic", giving the format of the
#' output
#'
#' @return quantiles: see `format`
#' @export
#'
#' @examples
#' md_quantile_values(
#'   welfare = md_GHI_2000_consumption$welfare,
#'   weight  = md_GHI_2000_consumption$weight,
#'   n       = 5
#' )
md_quantile_values <- function(
    welfare    = NULL,
    weight     = rep(1, length = length(welfare)),
    n          = 10,
    format     = c("dt", "list", "atomic")
){

  # ____________________________________________________________________________
  # Arguments ------------------------------------------------------------------
  if (is.na(welfare) |> any()) {
    cli::cli_abort("No elements in welfare vector can be NA")
  }
  if (is.null(welfare)) {
    cli::cli_abort("Welfare vector cannot be NULL")
  }
  if (length(weight) > 1 & any(is.na(weight))) {
    cli::cli_abort("No elements in weight vector can be NA - make NULL to use equal weighting")
  }
  format <- match.arg(format) # Ask about this argument to Zander

  # ____________________________________________________________________________
  # Calculations ---------------------------------------------------------------
  lz <- md_compute_lorenz(welfare, weight, nbins = n)
  q  <- lz$welfare

  popshare <- seq(from = 1/n, to = 1, by = 1/n)
  names(q) <- paste0(popshare*100, '%')

  # ____________________________________________________________________________
  # Format and Return ----------------------------------------------------------
  if (format == "atomic") {
    return(q)
  } else if (format == "dt") {
    q <- data.table::data.table(
      quantile = paste0("q_", names(q)),
      values   = q |> as.numeric()
    )
    return(q)
  } else if (format == "list") {
    return(
      as.list(q)
    )
  }

}

#' Welfare share by quantile in micro data (old version)
#'
#' `old_md_welfare_share_at` returns the share of welfare held by the specified
#' share of the population in the parameter `popshare`. Alternatively, you can
#' select the number of quantiles (10 be default), to estimate the corresponding
#' share of welfare in each.
#'
#' @inheritParams old_md_quantile_values
#'
#' @return list with vector of share of welfare by quantiles
#' @export
#'
#' @examples
#' old_md_welfare_share_at(welfare = md_GHI_2000_consumption$welfare,
#'                     weight = md_GHI_2000_consumption$weight)
old_md_welfare_share_at <- function(
    welfare    = NULL,
    weight     = rep(1, length = length(welfare)),
    n          = NULL,
    popshare   = ifelse(is.null(n),
                        seq(from = 1/10, to = 1, by = 1/10),
                        seq(from = 1/n, to = 1, by = 1/n)),
    format     = c("dt", "list", "atomic")
){
  # ____________________________________________________________________________
  # Arguments ------------------------------------------------------------------
  if (is.na(welfare) |> any()) {
    cli::cli_abort("No elements in welfare vector can be NA")
  }
  if (is.null(welfare)) {
    cli::cli_abort("Welfare vector cannot be NULL")
  }
  if (length(weight) > 1 & any(is.na(weight))) {
    cli::cli_abort("No elements in weight vector can be NA - make NULL to use equal weighting")
  }
  if (is.null(n) & is.null(popshare)) {
    cli::cli_abort("Either `n` or `popshare` must be non-NULL")
  }
  format <- match.arg(format)

  # ____________________________________________________________________________
  # Validate n ----------------------------------------------------------
  if (!is.null(n)) {
    popshare <- seq(from = 1/n, to = 1, by = 1/n)
  }

  # ----------------------------------------------------------------------------
  # Validate popshare ----------------------------------------------------------
  if (!is.null(popshare)) {
    if (any(popshare < 0 | popshare > 1)) {
      cli::cli_abort("popshare must be within the range [0, 1]")
    }
  }

  # ____________________________________________________________________________
  # Calculations ---------------------------------------------------------------

  # Get quantiles
  q       <- old_md_quantile_values(
    welfare  = welfare,
    weight   = weight,
    n        = n,
    popshare = popshare,
    format   = "list"
  )


  if (length(funique(unlist(unname(q)))) < length(unlist(unname(q)))) {
    cli::cli_alert_warning(
      "Some quantile threshold values are equal. Please either reduce `n`,
      investigate `welfare` and `weight` vectors, or
      check using `md_quantile_values`."
    )
  }

  # Get total welfare, and order other vecs
  total_welfare <- fsum(x = welfare,
                        w = weight)
  weight        <- weight[order(welfare)]
  welfare       <- welfare[order(welfare)]

  # Weighted welfare shares
  output <- lapply(q,
                     \(y){
                       fsum(x = welfare[welfare <= y],
                            w = weight[welfare <= y]) / total_welfare
                     })

  # ____________________________________________________________________________
  # Format & Return -------------------------------------------------------------
  if (format == "list") {
    return(output)
  } else if (format == "atomic") {
    return(
      output |> unlist()
    )
  } else if (format == "dt") {
    output <- data.table(
      quantile   = paste0("q_", names(output)),
      share_at   = output |> as.numeric()
    )
    return(output)
  }

}

#' Welfare share by quantile in micro data
#'
#' `md_welfare_share_at` returns the share of welfare held by an specified
#' share of the population. You can select the number of quantiles (10 be default).
#' This function makes use of `md_compute_lorenz`.
#'
#'
#' @inheritParams md_quantile_values
#'
#'
#' @return list with vector of share of welfare by quantiles
#' @export
#'
#' @examples
#' md_welfare_share_at(welfare = md_GHI_2000_consumption$welfare,
#'                     weight = md_GHI_2000_consumption$weight)
md_welfare_share_at <- function(
    welfare    = NULL,
    weight     = rep(1, length = length(welfare)),
    n          = 10,
    format     = c("dt", "list", "atomic")
){
  # ____________________________________________________________________________
  # Arguments ------------------------------------------------------------------
  if (is.na(welfare) |> any()) {
    cli::cli_abort("No elements in welfare vector can be NA")
  }
  if (is.null(welfare)) {
    cli::cli_abort("Welfare vector cannot be NULL")
  }
  if (length(weight) > 1 & any(is.na(weight))) {
    cli::cli_abort("No elements in weight vector can be NA - make NULL to use equal weighting")
  }

  format <- match.arg(format)

  # ____________________________________________________________________________
  # Calculations ---------------------------------------------------------------

  # Get quantiles
  lz            <- md_compute_lorenz(
    welfare  = welfare,
    weight   = weight,
    nbins    = n)
  output        <- lz$lorenz_welfare
  popshare      <- seq(from = 1/n, to = 1, by = 1/n)
  names(output) <- paste0(popshare*100, '%')

  # ____________________________________________________________________________
  # Format & Return -------------------------------------------------------------
  if (format == "list") {
    return(output)
  } else if (format == "atomic") {
    return(
      output |> unlist()
    )
  } else if (format == "dt") {
    output <- data.table(
      quantile   = paste0("q_", names(output)),
      share_at   = output |> as.numeric()
    )
    return(output)
  }

}




#' Quantile welfare share (old_version)
#'
#' `old_md_quantile_welfare_share` returns the share of welfare held by a
#' particular quantile. Notice that `md_welfare_share_at` get the share of
#' welfare held by a particular share of the population, which is in a sense
#' the cumulative share. Instead, `md_quantile_welfare_share` returns
#' the proportion of welfare that only the specified quantile holds.
#'
#' @inheritParams old_md_quantile_values
#'
#' @return list with vector of share of welfare by quantiles
#' @export
#'
#' @examples
#' old_md_quantile_welfare_share(welfare = md_GHI_2000_consumption$welfare,
#'                              weight = md_GHI_2000_consumption$weight)
old_md_quantile_welfare_share <- function(
    welfare    = NULL,
    weight     = rep(1, length = length(welfare)),
    n          = NULL,
    popshare   = ifelse(is.null(n),
                        seq(from = 1/10, to = 1, by = 1/10),
                        seq(from = 1/n, to = 1, by = 1/n)),
    format     = c("dt", "list", "atomic")
){
  # ____________________________________________________________________________
  # Arguments ------------------------------------------------------------------
  if (is.na(welfare) |> any()) {
    cli::cli_abort("No elements in welfare vector can be NA")
  }
  if (is.null(welfare)) {
    cli::cli_abort("Welfare vector cannot be NULL")
  }
  if (length(weight) > 1 & any(is.na(weight))) {
    cli::cli_abort("No elements in weight vector can be NA - make NULL to use equal weighting")
  }
  if (is.null(n) & is.null(popshare)) {
    cli::cli_abort("Either `n` or `popshare` must be non-NULL")
  }
  if (length(unique(welfare)) == 1) {
    cli::cli_abort("The `welfare` vector should have more than one unique values")
  }
  format <- match.arg(format)

  # ____________________________________________________________________________
  # Specify Quantiles ----------------------------------------------------------
  if (!is.null(n)) {
    popshare <- seq(from = 1/n, to = 1, by = 1/n)
  }
  weight  <- weight[order(welfare)]
  welfare <- welfare[order(welfare)]

  quantiles <- old_md_quantile_values(
    welfare  = welfare,
    weight   = weight,
    n        = n,
    popshare = popshare,
    format   = "atomic"
  )

  # ____________________________________________________________________________
  # Get welfare shares ---------------------------------------------------------
  total_sum <- fsum(welfare*weight)

  # Create a factor indicating the range of each element
  # Add a small epsilon to the max value
  quantiles <- c(-Inf, quantiles)
  if (!fmax(quantiles) == fmax(welfare)) {
    quantiles <- c(quantiles, fmax(welfare) + .Machine$double.eps)
  }

  quantile_groups <- cut(welfare, breaks = quantiles)
  welfare_split   <- split(welfare, quantile_groups)
  weight_split    <- split(weight, quantile_groups)

  shares <- sapply(seq_along(welfare_split), function(i) {
    fsum(x = welfare_split[[i]],
         w = weight_split[[i]])
  })

  # Calculate the share of each category
  shares        <- shares / total_sum
  names(shares) <- paste0(popshare*100, "%")

  if (is.null(n)) {
    shares <- shares[1]
  }

  # ____________________________________________________________________________
  # Format & Return -------------------------------------------------------------
  if (format == "list") {
    return(shares |> as.list())
  } else if (format == "atomic") {
    return(shares)
  } else if (format == "dt") {
    shares <- data.table::data.table(
      quantile   = paste0("q_", names(shares)),
      share_at   = shares |> as.numeric()
    )
    return(shares)
  }

}



#' Quantile welfare share
#'
#' `md_quantile_welfare_share` returns the share of welfare held by a
#' particular quantile. Notice that `md_welfare_share_at` get the share of
#' welfare held by a particular share of the population, which is in a sense
#' the cumulative share. Instead, `md_quantile_welfare_share` returns
#' the proportion of welfare that only the specified quantile holds.
#'
#' @inheritParams md_quantile_values
#'
#' @return list with vector of share of welfare by quantiles
#' @export
#'
#' @examples
#' md_quantile_welfare_share(welfare = md_GHI_2000_consumption$welfare,
#'                              weight = md_GHI_2000_consumption$weight)
md_quantile_welfare_share <- function(
    welfare    = NULL,
    weight     = rep(1, length = length(welfare)),
    n          = 10,
    format     = c("dt", "list", "atomic")
){
  # ____________________________________________________________________________
  # Arguments ------------------------------------------------------------------
  if (is.na(welfare) |> any()) {
    cli::cli_abort("No elements in welfare vector can be NA")
  }
  if (is.null(welfare)) {
    cli::cli_abort("Welfare vector cannot be NULL")
  }
  if (length(weight) > 1 & any(is.na(weight))) {
    cli::cli_abort("No elements in weight vector can be NA - make NULL to use equal weighting")
  }
  if (length(unique(welfare)) == 1) {
    cli::cli_abort("The `welfare` vector should have more than one unique values")
  }
  format <- match.arg(format)

  # ____________________________________________________________________________
  # Get welfare shares ---------------------------------------------------------
  lz            <- md_compute_lorenz(
    welfare  = welfare,
    weight   = weight,
    nbins    = n)
  shares        <- diff(c(0,lz$lorenz_welfare))
  popshare      <- seq(from = 1/n, to = 1, by = 1/n)
  names(shares) <- paste0(popshare*100, '%')

  # ____________________________________________________________________________
  # Format & Return -------------------------------------------------------------
  if (format == "list") {
    return(shares |> as.list())
  } else if (format == "atomic") {
    return(shares)
  } else if (format == "dt") {
    shares <- data.table::data.table(
      quantile   = paste0("q_", names(shares)),
      share_at   = shares |> as.numeric()
    )
    return(shares)
  }

}










