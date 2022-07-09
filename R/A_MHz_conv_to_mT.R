#
#' Convert Coupling Constants (\eqn{A}, HFCCs in MHz) to Hyperfine Splittings (\eqn{a}, HFSCs in mT).
#'
#'
#' @description Converts hyperfine coupling constants HFCCs (\eqn{A} values in \code{MHz})
#' to hyperfine splitting constants HFSCs (\eqn{a} values in \code{mT})
#'
#'
#' @param A.MHz Numeric value/vector corresponding to HFCCs in \code{MHz}
#' @param g Numeric value/vector corresponding to actual \eqn{g}-factor (\code{unitless}),
#' \strong{default:} \code{g = 2.00231930} (free electron)
#'
#'
#' @returns Numeric value/vector corresponding to HFSCs (\eqn{a}) in \code{mT})
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
  return(round(a,digits = 2))
  #
}
