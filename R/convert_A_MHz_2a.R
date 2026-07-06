#
#' Convert Coupling Constants into Splitting Ones.
#'
#'
#' @family Conversions and Corrections
#'
#'
#' @description Converting hyperfine coupling constants (HFCCs, \eqn{A} values in \code{MHz})
#'   into hyperfine splitting ones (HFSCs, \eqn{a} values in \code{mT}).
#'
#'
#' @details
#'   Conversion performed according to the following relation:
#'   \deqn{a = A\,h\,10^{6}/(g\,\mu_{\text{B}}\,10^{-3})}
#'   where \eqn{h} corresponds to Planck's constant and \eqn{\mu_{\text{B}}} to Bohr's magneton.
#'   Both constants are obtained by the \code{constans::syms$h} and \code{constants::syms$mub}, respectively,
#'   using the \href{https://r-quantities.github.io/constants/}{constants} package (see \code{\link[constants]{syms}}).
#'   The \eqn{10^{6}\,/\,10^{-3}} factor is introduced due to specific \eqn{[A] = \text{MHz}} \eqn{\rightarrow}
#'   \eqn{[a] = \text{mT}} conversion. The latter is suitable for the EPR simulations and/or ENDOR.
#'
#'
#'
#' @param A.MHz Numeric value/vector, corresponding to HFCCs in \code{MHz}.
#' @param g.val Numeric value/vector, corresponding to actual \eqn{g}-factor (\code{unitless}).
#'   \strong{Default:} \code{g.val = 2.00231930} (corresponding to free electron).
#'
#'
#' @returns Numeric value/vector corresponding to HFSCs (\eqn{a}) in \code{mT}.
#'
#'
#' @examples
#' # all a (HFSCs) in mT
#' convert_A_MHz_2a(A.MHz = 16)
#' #
#' convert_A_MHz_2a(20,2.0059) # 20 MHz
#' #
#' convert_A_MHz_2a(4,g.val = 2.0036) # 4 MHz
#'
#'
#' @export
#'
#'
#' @importFrom constants syms
convert_A_MHz_2a <- function(A.MHz,g.val = 2.00231930){
  #
  Planck.const <- syms$h
  Bohr.magnet <- syms$mub
  a <- (A.MHz*Planck.const*1e+6)/(g.val*Bohr.magnet*1e-3)
  #
  return(round(a,digits = 2))
  #
}
