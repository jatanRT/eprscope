#
#' Convert Splitting Constants (\eqn{a} in mT) to Coupling ones (\eqn{A} in MHz).
#'
#'
#' @description Converts hyperfine splitting constants HFSCs (\eqn{a} values in \code{mT})
#'   to hyperfine coupling constants HFCCs (\eqn{A} values in \code{MHz}) according to following
#'   relation:
#'   \deqn{A = (a\,g\,\mu_{\text{B}}) / h}
#'
#'
#' @param a.mT Numeric value/vector of HFSCs in \code{mT} ('line distances' from EPR spectrum)
#' @param g Numeric value/vector corresponding to actual \eqn{g}-factor (\code{unitless}).
#' \strong{Default:} \code{g = 2.00231930} (corresp. to free electron)
#'
#'
#' @returns Numeric value/vector corresponding to HFCCs (\eqn{A}) in \code{MHz})
#'
#'
#' @examples
#' convert_a_mT_2A(a.mT = 0.5)
#' convert_a_mT_2A(0.6,2.0059)
#' convert_a_mT_2A(0.15,g = 2.00036)
#'
#'
#' @export
#'
#'
convert_a_mT_2A <- function(a.mT,g = 2.00231930){
  #
  Planck.const <- syms$h
  Bohr.magnet <- syms$mub
  A <- (g*Bohr.magnet*1e-3*a.mT)/(Planck.const*1e+6)
  #
  return(round(A,digits = 3))
  #
}
