#' Title
#'
#' @param A numeric:
#' @param B
#' @param C
#' @return
#' @export
#'
#' @examples
get_components_lq <- function(A,B,C){

  e   <-  -(A + B + C + 1)
  m   <-  (B^2) - (4 * A)
  n   <-  (2 * B * e) - (4 * C)
  r   <-  sqrt(n^2 - 4 * m * e^2)
  s1  <-  (r - n) / (2 * m)
  s2  <-  -(r + n) / (2 * m)


  return(list(
    e  =  e ,
    m  =  m ,
    n  =  n ,
    r  =  r ,
    s1 =  s1,
    s2 =  s2
  ))
}
