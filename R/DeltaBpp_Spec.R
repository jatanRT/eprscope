#
#'
#' Calculation of Spectral Linewidth (\eqn{\Delta B_{pp}}) of the EPR Spectrum
#'
#'
#' @description
#' The function calculates the \eqn{\Delta B_{pp}} (peak-to-peak) linewidth
#' of EPR spectrum. The difference corresponds to magnetic flux densities corresponding
#' to minimum and maximum of the \code{dIepr_over_dB} in the selected \code{B} region.
#'
#'
#' @param data.spectrum Spectrum data frame/table with magnetic flux density (in \code{mT} or \code{G}) column
#'   must be labeled as \code{B_mT} (or \code{B_G}) and that of the derivative intensity as \code{dIepr_over_dB},
#'   \code{index} column may be included as well
#' @param B Character/String pointing to magnetic flux density \code{column} of EPR spectrum data frame
#'   \code{data.spectrum} either in \code{millitesla} or in \code{Gauss}, that is \code{B = "B_mT"} (\strong{default})
#'   or \code{B = "B_G"} or \code{B = "B_G_Sim"} to include simulated EPR spectra as well
#' @param Intensity Character/String pointing to \code{intensity column} if other than \code{dIepr_over_dB}
#'   name/label is used (e.g. for simulated spectra), \strong{default}: \code{Intesity = "dIepr_over_dB"}
#' @param Blim Numeric vector, magnetic flux density in \code{mT}/\code{G} corresponding to border limits
#'   of the selected \eqn{B} region, e.g. like `Blim = c(349.54,359.54)`
#'
#'
#' @return NUmeric Value of \eqn{B} difference (the absolute value) corresponding to \code{minimum}
#'   and \code{maximum} of the derivative intensity (\code{dIepr_over_dB}) => \eqn{ \Delta B_{pp}}
#'
#'
#' @examples
#' \dontrun{
#' DeltaBpp_Spec(data.spectrum,c(320.221,328.331))
#' DeltaBpp_Spec(data.spectrum,B = "B_G",Intensity = "dIepr_over_dB",c(3202.11,3283.31))
#' DeltaBpp_Spec(data.spectrum,"B_mT",Blim = c(320.221,328.331))
#' DeltaBpp_Spec(data.spectrum,"B_mT_Sim",c(320.221,328.331))
#' }
#'
#' @export
#'
#'
DeltaBpp_Spec <- function(data.spectrum,
                          B = "B_mT",
                          Intensity = "dIepr_over_dB",
                          Blim){
  #
  ## B corresponding to minimum and maximum derivative intensities
  ## in the selected B region ('B.reg.'):
  B.min <- data.spectrum %>%
    filter(between(.data[[B]],Blim[1],Blim[2])) %>%
    filter(.data[[Intensity]] == min(.data[[Intensity]])) %>%
    pull(.data[[B]])
  #
  B.max <- data.spectrum %>%
    filter(between(.data[[B]],Blim[1],Blim[2])) %>%
    filter(.data[[Intensity]] == max(.data[[Intensity]])) %>%
    pull(.data[[B]])
  #
  ## Delta_B calculation:
  DeltaB_pp <- abs(B.min - B.max)
  #
  return(round(DeltaB_pp,digits = 2))
  #
}
