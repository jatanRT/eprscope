#
#' @title TODO
#'
#'
#' @description TODO
#'
#'
#' @param a TODO
#' @param g TODO
#'
#'
#' @return TODO
#'
#'
#' @examples
#' TODO
#' TODO
#'
#'
#' @export
#'
#'
a_mT_convert_to_MHz <- function(a,g){
  Planck.const <- syms$h
  Bohr.magnet <- syms$muB
  A <- (g*Bohr.magnet*1e-3*a)/(Planck.const*1e+6)
  return(A)
}
