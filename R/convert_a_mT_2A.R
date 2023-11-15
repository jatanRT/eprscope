#
#' Convert Splitting Constants (\eqn{a} in mT) to Coupling ones (\eqn{A} in MHz).
#'
#'
#' @family Conversions and Corrections
#'
#'
#' @description Converting hyperfine splitting constants (HFSCs, \eqn{a} values in `mT`)
#'   into hyperfine coupling constants (HFCCs, \eqn{A} values in `MHz`).
#'
#'
#' @details
#'   Conversion proceeds according to followign relation:
#'   \deqn{A = (a\,g\,\mu_{\text{B}}) / h}
#'   where \eqn{h} corresponds to Planck's constant and \eqn{\mu_{\text{B}}} to Bohr's magneton.
#'   Both latter were obtained by \code{constans::syms$h} and \code{constants::syms$mub}, respectively
#'   from the \pkg{constants} package. Conversion is suitable for EPR simulations and/or ENDOR.
#'
#'
#' @param a.mT Numeric value/vector of HFSCs in `mT` ('line distances' from EPR spectrum)
#' @param g.val Numeric value/vector corresponding to actual \eqn{g}-factor (\code{unitless}).
#' \strong{Default:} \code{g.val = 2.00231930} (corresponding to free electron).
#'
#'
#' @returns Numeric value/vector corresponding to HFCCs (\eqn{A}) in \code{MHz}).
#'
#'
#' @examples
#' convert_a_mT_2A(a.mT = 0.5)
#' convert_a_mT_2A(0.6,2.0059)
#' convert_a_mT_2A(0.15,g.val = 2.00036)
#'
#'
#' @export
#'
#'
convert_a_mT_2A <- function(a.mT,g.val = 2.00231930){
  #
  Planck.const <- syms$h
  Bohr.magnet <- syms$mub
  A <- (g.val*Bohr.magnet*1e-3*a.mT)/(Planck.const*1e+6)
  #
  return(round(A,digits = 3))
  #
}
