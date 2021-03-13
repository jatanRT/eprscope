#
#' @title Calculation of \eqn{g}-factor ("Position") from the EPR Spectrum
#'
#' @description Calculation of g-value according to fundamental formula. \eqn{g}-related magnetic flux density
#'   (like \eqn{B_{iso}} or \eqn{B_{center}}) is directly taken from the EPR spectrum.
#'   If positive and negative derivative intensities of the spectral line are similar, \eqn{B_{iso}} should be
#'   be considered, otherwise the \eqn{B_{center}} must be taken into account to calculate the \eqn{g}-value.
#'   The \eqn{g}-related \eqn{B} is computed either as the mid-point between the magnetic flux densities
#'   corresponding to \code{min.} and \code{max.} derivative intensities (\code{dIepr_over_dB})
#'   or by \eqn{B}-value corresponding to \eqn{dIepr_over_dB} very close to zero.
#'   One can select the B region/span/interval from the spectrum to determine the \eqn{g}-value.
#'   The Planck constant (\eqn{h}) and the Bohr magneton (\eqn{\mu_{B}}) are included
#'   in \code{\link[constants]{syms}} function and their values are taken by the \code{syms$h}
#'   and \code{syms$muB} commands, respectively.
#'
#' @param spectrum.data Spectrum data frame/table where the magnetic flux density (in \code{mT} or ) column
#'   must be labeled as \code{B_mT} and that of the derivative intensity as \code{dIepr_over_dB},
#'   \code{index} column can be included as well
#' @param nu Numeric, microwave frequency in \code{GHz}
#' @param B Character/String pointing to magnetic flux density \code{column} of EPR spectrum data frame
#'   \code{spectrum.data} either in \code{millitesla} or in \code{Gauss}, that is \code{B = "B_mT"} (default)
#'   or \code{B = "B_G"}
#' @param B.reg.start Numeric, magnetic flux density in \code{mT} corresponding to \code{starting} border
#'   of the \code{selected B region} (therefore abbreviation \code{.reg.})
#' @param B.reg.end Numeric, magnetic flux density in \code{mT} corresponding to \code{ending} border
#'   of the \code{selected B region} (therefore abbreviation \code{.reg.})
#' @param iso Boolean, whether to calculate the \eqn{g}-factor from the \eqn{B} value corresponding to
#'   that between the \code{min.} and \code{max.} derivative intensities (\code{dIepr_over_dB}, that is \eqn{g_{iso}}
#'   (this is the \code{default: iso = TRUE}), or by finding the the \eqn{B} value corresponding
#'   to \code{dIepr_over_dB = 0} (close/near zero, which is \code{iso = FALSE})
#'
#' @return \eqn{g_{iso}}-value ('iso' = 'isotropic') according to \eqn{(\nu h)/(\mu_{B} B)},
#'   where the \eqn{B} comes directly from the EPR spectrum and is actually calculated between
#'   the \code{\emph{B}(maximum)} and \code{\emph{B}(minumum)} (THESE DO NOT EQUAL to \code{B.reg.start}
#'   and \code{B.reg.min}!) corresponding to maximum and minimum of the derivative intensity (\code{dIepr_over_dB})
#'
#' @examples
#' \dontrun{
#' gValue_fromSpectr(spectrum.data,9.82451,"B_mT",349.8841,351.112)
#' gValue_fromSpectr(spectrum.data,nu = 9.82451,B = "B_G",B.reg.start = 3498.841,B.reg.end = 3511.12,iso = FALSE)
#' }
#'
#'
#' @export
#'
#'
#' @importFrom dplyr filter select mutate pull between near
gValue_fromSpectr <- function(spectrum.data,nu,B = "B_mT",B.reg.start,B.reg.end,iso = TRUE){
  ## B between minimum and maximum of dIepr_over_dB:
  if (isTRUE(iso)){
    B.min <- spectrum.data %>%
      filter(between(.data[[B]],B.reg.start,B.reg.end)) %>%
      filter(.data$dIepr_over_dB == min(.data$dIepr_over_dB)) %>%
      pull(.data[[B]])
    ## B at maximum of dIepr_over_dB:
    B.max <- spectrum.data %>%
      filter(between(.data[[B]],B.reg.start,B.reg.end)) %>%
      filter(.data$dIepr_over_dB == max(.data$dIepr_over_dB)) %>%
      pull(.data[[B]])
    ## B between both of them:
    B.center <- (B.min + B.max)/2
    ## B at dIepr_over_dB = 0 (near 0):
  } else{
    B.center <- spectrum.data %>%
      filter(between(.data[[B]],B.reg.start,B.reg.end)) %>%
      filter(near(.data$dIepr_over_dB,0)) %>%
      filter(.data$dIepr_over_dB == min(.data$dIepr_over_dB)) %>%
      pull(.data[[B]])
  }
  ## g -value calculation:
  Planck.const <- constants::syms$h
  Bohr.magnet <- constants::syms$muB
  g.precurs <- (Planck.const*nu*1e+9)/(Bohr.magnet*B.center)
  if (B == "B_mT"){
    g <- g.precurs/1e-3
  }
  if (B == "B_G"){
    g <- g.precurs/1e-4
  }
  return(round(g,digits = 5))
}
