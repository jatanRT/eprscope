#'
#'  ENDOR/Larmor Frequency of Specific Nucleus
#'
#'
#' @family Evaluations
#'
#'
#' @description
#'   Larmor/ENDOR frequency calculations for the analysis in EPR/Electron Nuclear Double Resonance (ENDOR).
#'   Function inspired by the similar \href{https://bio.groups.et.byu.net/LarmourFreqCal.phtml}{Larmor Frequency
#'   Calculator} available online.
#'
#'
#' @details
#'   The frequency in MHz is calculated according to relation
#'   \deqn{\nu_{\text{ENDOR}}^{} = - (1/h)\,\mu_{\text{N}}^{}\,g_{\text{n}}^{}\,B\,10^{-6}}
#'   where \eqn{h} is the Planck's constant, \eqn{\mu_{\text{N}}^{}} is the nuclear magneton
#'   available from \href{https://r-quantities.github.io/constants/}{constants} package (\code{constants::syms$mun}),
#'   \eqn{g_{\text{n}}^{}} is the nuclear \eqn{g}-factor of the specific nucleus (reported in the package
#'   \code{isotopes_ds} data frame as \code{g_Nuclear}) and finally, the \eqn{B} denotes the magnetic flux
#'   density at which the ENDOR spectra are recorded (see also \code{B.val} in arguments). The \eqn{10^{-6}} coefficient
#'   is referred to the resulting frequency in MHz. The negative sign "\eqn{-}" mirrors the convention to describe the direction
#'   of magnetic spin moments precession either counter-clockwise (\eqn{+}, if \eqn{\gamma_{\text{n}}^{} < 0})
#'   or clockwise (\eqn{-}, if \eqn{\gamma_{\text{n}}^{} > 0}, Levitt MH (2013)).
#'
#'
#' @references
#'   Levitt MH (2013). \emph{Spin Dynamics: Basics of Nuclear Magnetic Resonance}.
#'   Wiley, ISBN 978-1-118-68184-8, \url{https://books.google.cz/books?id=bysFAa4MPQcC}.
#'
#'
#' @param nucle_us_i (Vector) character string, in the form like \code{"14N"} or \code{c("1H","13C")},
#'   pointing to specific nucleus/nuclei for which the frequency should by calculated. The nuclear \emph{g}-factors
#'   for those nuclei are taken from the \code{\link{isotopes_ds}}.
#' @param B.unit Character string denoting the magnetic flux density \emph{B} unit. \strong{Default}:
#'   \code{B.unit = "G"}.
#' @param B.val Numeric, magnetic flux density \eqn{B}-\code{value}. This actually corresponds
#'   to \eqn{B} at which the EPR signal saturates to record the ENDOR spectrum/spectra.
#'
#'
#' @return Numeric value or vector of nuclear Larmor/ENDOR frequencies in MHz for selected
#'   nuclei at \eqn{B} = \code{B.val}.
#'
#'
#' @examples
#' ## Larmor/ENDOR frequency for one selected nucleus
#' ## only, e.g. "14N" at 3486 G
#' eval_nu_ENDOR(nucle_us_i = "14N",
#'               B.val = 3486)
#' #
#' ## Larmor/ENDOR frequency for selected nuclei
#' ## e.g. "1H" and "31P" at saturation
#' ## field of B = 349.9 mT
#' eval_nu_ENDOR(nucle_us_i = c("1H","31P"),
#'               B.unit = "mT",
#'               B.val = 349.9)
#'
#'
#' @export
#'
#'
eval_nu_ENDOR <- function(nucle_us_i,
                          B.unit = "G",
                          B.val) {
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
  if (B.unit == "T") {
    B.val <- B.val
  } else if (B.unit == "mT") {
    B.val <- B.val * 0.001
  } else if (B.unit == "G") {
    B.val <- B.val * 0.0001
  }
  #
  ## load the specific nucleus and its properties
  ## from the `isotopes_ds` and select its g-value: `g_Nuclear`
  ## for "vectorized" nuclei
  if (length(nucle_us_i) > 1) {
    g_Nuclear.nucle.us.i <- sapply(
      seq(nucle_us_i),
      function(i) {
        eprscope::isotopes_ds %>%
          dplyr::filter(Isotope == nucle_us_i[i]) %>%
          dplyr::pull(g_Nuclear)
      }
    )
  } else {
    #
    ## just for one nucleus
    g_Nuclear.nucle.us.i <- eprscope::isotopes_ds %>%
      dplyr::filter(Isotope == nucle_us_i) %>%
      dplyr::pull(g_Nuclear)
  }
  #
  ## ENDOR frequency (in MHz) calculation:
  nu_ENDOR <- -(1 / Planck.const) *
    g_Nuclear.nucle.us.i * nuclear.mu * B.val * 1e-6
  #
  return(nu_ENDOR)
  #
}
