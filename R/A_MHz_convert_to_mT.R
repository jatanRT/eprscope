#
#' @title Conversion of Hyperfine Coupling Constants (\code{HFCCs}, \eqn{A} in \code{MHz}) to Hyperfine Splitting
#'   ones (\code{HFSCs}, \eqn{a} in \code{mT})
#'
#'
#' @description TODO
#'
#'
#' @param A Numeric value/Numeric vector corresponding to HFCCs in \code{MHz}
#' @param g Numeric value/Numeric vector corresponding to actual \eqn{g}-factor (\code{unitless})
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
