#' Title
#'
#' @param A
#' @param B
#' @param C
#' @param ct list of parametric quadratic Lorenz components (from `get_components_lq()`)
#' @param mu numeric: distributional mean
#' @param z numeric: Poverty line
#'
#' @return
#' @export
#'
#' @examples
fgt_lq <- function(A, B, C, mu, z, ct) {
  nct <- names(ct)
  for (i in seq_along(nct)) {
    assign(nct[i], ct[[i]])
  }

  # poverty headcount
  H <- - (1 / (2*m) ) * (n + r * (B + (2*z/mu))*((B + (2*z/mu))^2 - m)^(-1/2))

  # Poverty Gap
  p <- H
  Lp <- - (1/2) * (B*p + e + (m*p^2 + n*p +e^2)^(1/2))
  PG <- H - (mu/z)*Lp

  # print results
  cli::cli_alert_success("Poverty Headcount {.val {scales::percent(H, accuracy = .01)}}")
  cli::cli_alert_success("Poverty Gap {.val {scales::percent(PG, accuracy = .01)}}")

  return(list(
    headcount = H,
    pov_gap   = PG
  ))

}
