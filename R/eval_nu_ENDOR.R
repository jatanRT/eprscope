#'
#' ENDOR/Larmor Freqency \eqn{\nu_{\text{ENDOR}}} for a Specific Nucleus and Magnetic Flux Density \eqn{B}
#'
#'
#' @family Evaluations
#'
#'
#' @description
#' A short description...tbc...
#'
#'
#' @param nucleus Character string in form of e.g. \code{"14N"}...tbc.
#' @param B.unit Character string denoting the magnetic flux density \eqn{B} unit.
#' @param B.val Numeric, magnetic flux density \eqn{B} \code{value}.
#'
#'
#' @return Numeric value or vector...tbc...
#'
#'
#' @examples
#' \dontrun{
#' tbc
#' tbc
#' }
#'
#'
#' @export
#'
#'
eval_nu_ENDOR <- function(nucleus,
                          B.unit,
                          B.val){
  #
  ## 'Temporary' processing variables
  Isotope <- NULL
  g_Nuclear <- NULL
  #
  ## Physical constants
  Planck.const <- constants::syms$h
  nuclear.mu <- constants::syms$mun ## Nuclear magneton
  #
  ## converting `B.val`
  if (B.unit == "T"){
    B.val <- B.val
  } else if (B.unit == "mT"){
    B.val <- B.val * 0.001
  } else if (B.unit == "G"){
    B.val <- B.val * 0.0001
  }
  #
  ## load the specific nucleus and its properties
  ## from the `isotope_db` and select its g-value: `g_Nuclear`
  g_Nuclear.nucleus <- eprscope::isotope_db %>%
    dplyr::filter(Isotope == nucleus) %>%
    dplyr::pull(g_Nuclear)
  #
  ## ENDOR frequency (in MHz) calculation:
  nu_ENDOR <- - (1/Planck.const) *
    g_Nuclear.nucleus * nuclear.mu * B.val * 1e-6
  #
  return(nu_ENDOR)
  #
}
