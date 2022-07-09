#
#' @title Convert Hyperfine Splitting Constants (HFSCs in mT) to Hyperfine Coupling ones (HFCCs in MHz)
#'
#'
#' @description Converts HFSCs (\emph{a} values in \code{mT}) to HFCCs (\emph{A} values in \code{MHz})
#'
#'
#' @param a.mT Numeric value/vector corresponding to HFSCs in \code{mT} ('line distances' from EPR spectrum)
#' @param g Numeric value/vector corresponding to actual \emph{g}-factor (\code{unitless}), \strong{default:} \code{g = 2.00231930}
#'
#'
#' @return Numeric value/vector corresponding to HFCCs (\emph{A}) in \code{MHz})
#'
#'
#' @examples
#' a_mT_conv_to_MHz(a.mT = 0.5)
#' a_mT_conv_to_MHz(0.6,2.0059)
#' a_mT_conv_to_MHz(0.15,g = 2.00036)
#'
#'
#' @export
#'
#'
a_mT_conv_to_MHz <- function(a.mT,g = 2.00231930){
  #
  Planck.const <- syms$h
  Bohr.magnet <- syms$mub
  A <- (g*Bohr.magnet*1e-3*a.mT)/(Planck.const*1e+6)
  #
  return(A)
  #
}
