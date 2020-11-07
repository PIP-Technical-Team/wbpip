#' Title
#'
#' @param A
#' @param B
#' @param C
#' @param H numeric: Population share
#' @param mu numeric: distributional mean
#' @param ct list of parametric quadratic Lorenz components (from `get_components_lq()`)
#' @param method numeric: estimation method. Either 1 or 2. Default 1.
#'
#' @return
#' @export
#'
#' @examples
popshare_lq <- function(A, B, C, H, mu, ct, method = 1) {
  nct <- names(ct)
  for (i in seq_along(nct)) {
    assign(nct[i], ct[[i]])
  }

  #Isolating Z from headcount equantion in table 2 in Datt paper
  if (method == 1) {
    y <- ( (-2*m*H - n) / r)^2

    z <- - (mu/2)*((y*B - B + sqrt(y*m*(y-1)) )/(y-1))
  }

  if (method == 2) { # have not idea where it comes from.
    x <- H
    tmp <- (m * x^2) + (n * x) + (e^2)
    tmp <- ifelse(tmp < 0, 0, tmp)

    # Formula for first derivative of GQ Lorenz Curve
    val <- -(0.5 * B) - (0.25 * (2 * m * x + n) / sqrt(tmp))
    z <- val*mu
  }
  p <- H
  Lp <- - (1/2) * (B*p + e + (m*p^2 + n*p +e^2)^(1/2))

  cli::cli_alert_success("Share of Popluation {.val {scales::percent(H, accuracy = .01)}}")
  cli::cli_alert_success("Share of total welfare, L(p) {.val {scales::percent(Lp, accuracy = .01)}}")
  cli::cli_alert_success("Threshold (poverty line) {.val {z}}")

  return(z)

}
