#
#' @title Conversion of Hyperfine Splitting Constants (\code{HFSCs}, \eqn{a} in \code{mT}) to Hyperfine Coupling
#'   ones (\code{HFCCs}, \eqn{A} in \code{MHz})
#'
#'
#' @description TODO
#'
#'
#' @param a.mT Numeric value/Numeric vector corresponding to HFSCs in \code{mT} ('line distances' from spectrum)
#' @param g Numeric value/Numeric vector corresponding to actual \eqn{g}-factor (\code{unitless})
#'
#'
#' @return Numeric Value/Vector corresponding to HFCCs (\eqn{A}) in \code{MHz})
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
a_mT_conv_to_MHz <- function(a.mT,g){
  #
  Planck.const <- syms$hbar*2*pi
  Bohr.magnet <- syms$mub
  A <- (g*Bohr.magnet*1e-3*a.mT)/(Planck.const*1e+6)
  #
  return(A)
  #
}
