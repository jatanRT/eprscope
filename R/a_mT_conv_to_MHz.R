#
#' @title Conversion of Hyperfine Splitting Constants (\code{HFSCs}, \eqn{a} in \code{mT}) to Hyperfine Coupling
#'   ones (\code{HFCCs}, \eqn{A} in \code{MHz})
#'
#'
#' @description TODO
#'
#'
#' @param a Numeric value/Numeric vector corresponding to HFSCs in \code{mT} ('line distances' from spectrum)
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
a_mT_conv_to_MHz <- function(a,g){
  Planck.const <- syms$hbar*2*pi
  Bohr.magnet <- syms$mub
  A <- (g*Bohr.magnet*1e-3*a)/(Planck.const*1e+6)
  return(A)
}
