#
#' @title TODO
#'
#'
#' @description TODO
#'
#'
#' @param A TODO
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
#' @importFrom constants syms
A_MHz_convert_to_mT <- function(A,g){
  Planck.const <- syms$hbar*2*pi
  Bohr.magnet <- syms$mub
  a <- (A*Planck.const*1e+6)/(g*Bohr.magnet*1e-3)
  return(a)
}
