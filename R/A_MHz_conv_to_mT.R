#
#' @title Convert Hyperfine Coupling Constants (HFCCs in MHz) to Hyperfine Splitting ones (HFSCs in mT)
#'
#'
#' @description Converts HFCCs (\emph{A} values in \code{MHz}) to HFSCs (\emph{a} values in \code{mT})
#'
#'
#' @param A.MHz Numeric value/vector corresponding to HFCCs in \code{MHz}
#' @param g Numeric value/vector corresponding to actual \emph{g}-factor (\code{unitless}), \strong{default:} \code{g = 2.00231930}
#'
#'
#' @return Numeric value/vector corresponding to HFSCs (\emph{a}) in \code{mT})
#'
#'
#' @examples
#' A_MHz_conv_to_mT(A.MHz = 16)
#' A_MHz_conv_to_mT(20,2.0059)
#' A_MHz_conv_to_mT(4,g = 2.00036)
#'
#'
#' @export
#'
#'
#' @importFrom constants syms
A_MHz_conv_to_mT <- function(A.MHz,g = 2.00231930){
  #
  Planck.const <- syms$h
  Bohr.magnet <- syms$mub
  a <- (A.MHz*Planck.const*1e+6)/(g*Bohr.magnet*1e-3)
  #
  return(a)
  #
}
