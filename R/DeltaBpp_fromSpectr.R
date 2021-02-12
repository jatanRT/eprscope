#
#' @title Calculation of Spectral Linewidth (\eqn{\Delta B_pp}) of the EPR Spectrum
#'
#'
#' @description The function calculates the \eqn{\Delta B_pp} (peak-to-peak) linewidth
#'   of EPR spectrum. The difference corresponds to magnetic flux densities corresponding
#'   to minimum and maximum of the \code{dIepr_over_dB} in the selected \code{B} region.
#'
#'
#' @param spectrum.data Spectrum data frame/table where the magnetic flux density (in \code{mT}) column
#'   must be labeled as \code{B_mT} and that of the derivative intensity as \code{dIepr_over_dB},
#'   \code{index} column can be included as well
#' @param B.reg.start Numeric, magnetic flux density in \code{mT} corresponding to \code{starting} border
#'   of the \code{selected B region} (therefore abbreviation \code{.reg.})
#' @param B.reg.end Numeric, magnetic flux density in \code{mT} corresponding to \code{ending} border
#'   of the \code{selected B region} (therefore abbreviation \code{.reg.})
#'
#'
#' @return Difference (the absolute value) of the magnetic flux densities (\code{B}) corresponding to \code{minimum}
#'   and \code{maximum} of the derivative intensity (\code{dIepr_over_dB}) => \eqn{\Delta B_pp}
#'
#'
#' @examples
#' \dontrun{
#' DeltaBpp_fromSpectr(spectrum.data,320.221,328.331)
#' DeltaBpp_fromSpectr(spectrum.data,B.reg.start = 320.221,B.reg.end = 328.331)
#' }
#'
#' @export
#'
#'
DeltaBpp_fromSpectr <- function(spectrum.data,B.reg.start,B.reg.end){
  ## B corresponding to minimum and maximum derivative intensities
  ## in the selected B region ('B.reg.'):
  B.min <- spectrum.data %>%
    filter(between(.data$B_mT,B.reg.start,B.reg.end)) %>%
    filter(.data$dIepr_over_dB == min(.data$dIepr_over_dB)) %>%
    pull(.data$B_mT)
  B.max <- spectrum.data %>%
    filter(between(.data$B_mT,B.reg.start,B.reg.end)) %>%
    filter(.data$dIepr_over_dB == max(.data$dIepr_over_dB)) %>%
    pull(.data$B_mT)
  ## Delta_B calculation:
  DeltaB_pp <- abs(B.min - B.max)
  return(round(DeltaB_pp,digits = 2))
}
