#
#' @title Calculation of the g-factor ("position") of EPR Spectrum
#'
#' @description Calculation of the g-value according to fundamental formula. g-related magnetic flux density
#'   (like \code{B_iso} or \code{B_center}) is directly taken from the symmetric
#'   (if positive and negative derivative intensities of the spectral line are similar)
#'   EPR spectrum. Otherwise the g will be determined innccurately.
#'   The g-related B is computed as the mid-point between the magnetic flux densities
#'   corresponding to \code{min.} and \code{max.} derivative intensities (\code{dIepr_over_dB}).
#'   One can select the B region/span/interval from the spectrum to determine the g-value.
#'   The Planck constant (\eqn{h}) and Bohr magneton (\eqn{\mu_B}) are included
#'   in \code{\link{constants}} package and their values are taken by \code{syms$h}
#'   and \code{syms$muB} commands, respectively.
#'
#' @param spectrum.data Spectrum data frame/table where the magnetic flux density (in \code{mT}) column
#'   must be labeled as \code{B_mT} and that of derivative intensity as \code{dIepr_over_dB},
#'   \code{index} column can be included as well
#' @param nu Microwave frequency in \code{GHz}
#' @param B.reg.start Magnetic flux density in \code{mT} corresponding to \code{starting} border
#'   of the \code{selected B region} (therefore abbreviation \code{.reg.})
#' @param B.reg.end Magnetic flux density in \code{mT} corresponding to \code{ending} border
#'   of the \code{selected B region} (therefore abbreviation \code{.reg.})
#'
#' @return \eqn{g_iso}-value ('iso' = 'isotropic') according to \eqn{(\nu * h)/(\mu_B * B)},
#'   where the \code{B} comes directly from the EPR spectrum and is actually calculated between
#'   the \code{B(maximum)} and \code{B(minumum)} (these do not equal to 'B.reg.start' and 'B.reg.min'!)
#'   corresponding to maximum and minimum of the derivative intensity (\code{dIepr_over_dB})
#'
#' @examples
#' \dontrun{
#' gValue_fromSpectr(spectrum.data,9.82451,349.8841,351.112)
#' gValue_fromSpectr(spectrum.data,nu = 9.82451,B.reg.start = 349.8841,B.reg.end = 351.112)
#' }
#'
#'
#' @export
#'
#'
#' @importFrom dplyr filter select mutate pull between near
gValue_fromSpectr <- function(spectrum.data,nu,B.reg.start,B.reg.end){
  ## B at minimum of dIepr_over_dB:
  B.min <- spectrum.data %>% filter(between(.data$B_mT,B.reg.start,B.reg.end)) %>%
    filter(.data$dIepr_over_dB == min(.data$dIepr_over_dB)) %>% pull(.data$B_mT)
  ## B at maximum of dIepr_over_dB:
  B.max <- spectrum.data %>% filter(between(.data$B_mT,B.reg.start,B.reg.end)) %>%
    filter(.data$dIepr_over_dB == max(.data$dIepr_over_dB)) %>% pull(.data$B_mT)
  ## B between both of them:
  B.center <- (B.min+B.max)/2
  ## g -value calculation:
  Planck.const <- constants::syms$h
  Bohr.magnet <- constants::syms$muB
  g <- (Planck.const*nu*1e+9)/(Bohr.magnet*B.center*0.001)
  return(round(g,digits = 5))
}
