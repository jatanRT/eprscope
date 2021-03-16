#
#'
#' @title Calculation of Spectral Linewidth (\eqn{\Delta B_{pp}}) of the EPR Spectrum
#'
#'
#' @description The function calculates the \eqn{\Delta B_{pp}} (peak-to-peak) linewidth
#'   of EPR spectrum. The difference corresponds to magnetic flux densities corresponding
#'   to minimum and maximum of the \code{dIepr_over_dB} in the selected \code{\emph{B}} region.
#'
#'
#' @param spectrum.data Spectrum data frame/table with magnetic flux density (in \code{mT} or \code{G}) column
#'   must be labeled as \code{B_mT} (or \code{B_G}) and that of the derivative intensity as \code{dIepr_over_dB},
#'   \code{index} column may be included as well
#' @param B Character/String pointing to magnetic flux density \code{column} of EPR spectrum data frame
#'   \code{spectrum.data} either in \code{millitesla} or in \code{Gauss}, that is \code{B = "B_mT"} (default)
#'   or \code{B = "B_G"}
#' @param B.reg.start Numeric, magnetic flux density in \code{mT} (\code{G}) corresponding to \code{starting} border
#'   of the \code{selected \emph{B} region} (therefore abbreviation \code{.reg.})
#' @param B.reg.end Numeric, magnetic flux density in \code{mT} (\code{G}) corresponding to \code{ending} border
#'   of the \code{selected \emph{B} region} (therefore abbreviation \code{.reg.})
#'
#'
#' @return Difference (the absolute value) of the magnetic flux densities (\code{B}) corresponding to \code{minimum}
#'   and \code{maximum} of the derivative intensity (\code{dIepr_over_dB}) => \eqn{ \Delta B_{pp}}
#'
#'
#' @examples
#' \dontrun{
#' DeltaBpp_fromSpectr(spectrum.data,320.221,328.331)
#' DeltaBpp_fromSpectr(spectrum.data,B = "B_G",3202.11,3283.31)
#' DeltaBpp_fromSpectr(spectrum.data,"B_mT",B.reg.start = 320.221,B.reg.end = 328.331)
#' }
#'
#' @export
#'
#'
DeltaBpp_fromSpectrum <- function(spectrum.data,B = "B_mT",B.reg.start,B.reg.end){
  ## B corresponding to minimum and maximum derivative intensities
  ## in the selected B region ('B.reg.'):
  B.min <- spectrum.data %>%
    filter(between(.data[[B]],B.reg.start,B.reg.end)) %>%
    filter(.data$dIepr_over_dB == min(.data$dIepr_over_dB)) %>%
    pull(.data[[B]])
  B.max <- spectrum.data %>%
    filter(between(.data[[B]],B.reg.start,B.reg.end)) %>%
    filter(.data$dIepr_over_dB == max(.data$dIepr_over_dB)) %>%
    pull(.data[[B]])
  ## Delta_B calculation:
  DeltaB_pp <- abs(B.min - B.max)
  return(round(DeltaB_pp,digits = 2))
}
