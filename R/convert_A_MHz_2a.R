#
#' Convert Coupling Constants (\eqn{A} in MHz) to Splitting ones (\eqn{a} in mT).
#'
#'
#' @description Converts hyperfine coupling constants HFCCs (\eqn{A} values in \code{MHz})
#'   to hyperfine splitting constants HFSCs (\eqn{a} values in \code{mT}) according to following
#'   relation:
#'   \deqn{a = \frac{A\,h}{g\,\mu_{\text{B}}}}
#'
#'
#' @param A.MHz Numeric value/vector corresponding to HFCCs in \code{MHz}
#' @param g Numeric value/vector corresponding to actual \eqn{g}-factor (\code{unitless}).
#'   \strong{Default:} \code{g = 2.00231930} ( corresp. to free electron)
#'
#'
#' @returns Numeric value/vector corresponding to HFSCs (\eqn{a}) in \code{mT})
#'
#'
#' @examples
#' convert_A_MHz_2a(A.MHz = 16)
#' convert_A_MHz_2a(20,2.0059)
#' convert_A_MHz_2a(4,g = 2.00036)
#'
#'
#' @export
#'
#'
#' @importFrom constants syms
convert_A_MHz_2a <- function(A.MHz,g = 2.00231930){
  #
  Planck.const <- syms$h
  Bohr.magnet <- syms$mub
  a <- (A.MHz*Planck.const*1e+6)/(g*Bohr.magnet*1e-3)
  #
  return(round(a,digits = 2))
  #
}
